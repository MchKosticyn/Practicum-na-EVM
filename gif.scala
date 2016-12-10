/**
  * Created by Asus on 05.12.2016.
  */

package GIFReader
import scodec._
import bits._
import codecs._
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}

class DataPanel(width : Int, height : Int,
                frame : Frame, scale : Int, BGColor : Color) extends Panel {
  override def paintComponent(g : Graphics2D) {
    g.setClip(0, 0, scale * width, scale * height)
    g.setBackground(BGColor)

    val dx, dy = scale

    frame.paintFrame(g, dx, dy)
  }
}

object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}


abstract class AnyDecoder {
  def splitByteBits(bits : BitVector): (Int, BitVector) = {
    val (byte, rest) = bits.splitAt(8)
    (byte.toInt(false), rest)
  }

  val Trailer = BitVector(hex"3b")
  val BlockTerminator = BitVector(hex"00")
  val ConstTerminator = constant(BlockTerminator)
  val IntTerminator = BlockTerminator.toInt()
  val NullBitVector = BitVector(Nil)
}


case class GIFHeader(Width : Int, Height : Int,
                     GCTF : Int,
                     ColorResolution : Int,
                     SortFlag : Int,
                     GCTsize : Int,
                     BGColorIndex : Int,
                     PXAspectRatio : Int) {
  def toHeader(colorTable : ColorTable): Header =
    new Header(Width, Height, colorTable(BGColorIndex))
}

case class Header(Width : Int, Height : Int, BGColor : Color)


abstract class GCEAbstract {
  val DelayTime : Int
  def isEmpty : Boolean
}

case class GCEBlock(Nothing : Int, DisposalMethod : Int,
                    UserInputFlag : Int,
                    TransparentColorFlag : Int,
                    DelayTime : Int,
                    TColorIndex : Int) extends GCEAbstract {
  def isEmpty = false
}

object EmptyGCEBlock extends GCEAbstract {
  val DelayTime = 100
  def isEmpty = true
}


case class ImageInfo(LeftPos : Int, TopPos : Int,
                     Width : Int, Height : Int,
                     LCTF : Int,
                     InterlaceF : Int,
                     SortF : Int, Nothing : Int,
                     LCTsize : Int)


class ColorTable(raw_bits : BitVector) {
  private def byteVectorToColor(x : ByteVector): Color = x.toArray match {
    case Array(r, g, b) =>
      new Color((r + 256) % 256, (g + 256) % 256, (b + 256) % 256)
  }

  val table: Array[Color] =
    raw_bits.bytes.grouped(3).toArray.map(byteVectorToColor)

  def apply(i : Int): Color = table(i) //--

  override def toString =
    table.foldLeft("ColorTable[")({(x, y) => x + y.toString + ", "})
}

class LCT(raw_bits : BitVector) extends ColorTable(raw_bits)

class GCT(raw_bits : BitVector) extends ColorTable(raw_bits)


class CodeTable(tableSize : Int, colorTable : ColorTable) {
  var table : ArrayBuffer[Array[Color]] =
    colorTable.table.grouped(1).to[ArrayBuffer] ++
      Array(Array(new Color(0, 0, 0)), Array(new Color(0, 0, 0)))

  def apply(i : Int): Array[Color] = table(i)

  def add(seq : Array[Color]) { table.append(seq) }

  def isDefinedAt(i : Int): Boolean = table.isDefinedAt(i)

  def reinit {
    table = colorTable.table.grouped(1).to[ArrayBuffer] ++
      Array(Array(new Color(0, 0, 0)), Array(new Color(0, 0, 0)))
  }

  def exceeds(codeSize : Int): Boolean =
    (table.length == (1 << codeSize))
}

class BitRemainder(var stream : BitVector, var remainder : BitVector) {
  override def toString = "BR: " + stream.toString + ", " + remainder.toString
}

object LZWdecoder extends AnyDecoder {
  def splitMuchBits(much : Int, bits : BitRemainder,
                    terminator : BitVector = NullBitVector,
                    EOIcode : Int = 0): Int = {
    while (bits.remainder.length < much) {
      val (byte, rest) = bits.stream.splitAt(8)
      if (byte == terminator) {
        bits.remainder = NullBitVector
        return EOIcode
      } else {
        bits.stream = rest
        bits.remainder = byte ++ bits.remainder
      }
    }
    val out = bits.remainder.takeRight(much).toInt(false)
    bits.remainder = bits.remainder.dropRight(much)
    out
  }


  def takeFirst(much : Int, bits : BitRemainder): Int =
    splitMuchBits(much, bits, NullBitVector)


  def parseLZWtable(bits : BitVector, colorTable : ColorTable):
  (ArrayBuffer[Color], BitVector) = {

    var (codeSize, butLZWminCodeSizeBits) = splitByteBits(bits)
    codeSize += 1
    val LZWcodeSize = codeSize

    val (blockSize, rawBits) = splitByteBits(butLZWminCodeSizeBits)
    val bitsRemainder = new BitRemainder(rawBits, NullBitVector)
    val clearCode = takeFirst(codeSize, bitsRemainder)
    val EOIcode = clearCode + 1
    val firstCode = splitMuchBits(codeSize, bitsRemainder)

    val codeTable = new CodeTable(LZWcodeSize, colorTable)

    def iterBlock(blockSize : Int,
                  leftBits : BitRemainder,
                  prevCode : Int,
                  lzwArray : ArrayBuffer[Color],
                  codeSize : Int,
                  takenBitsOfFirstBlock : Int = 0
                 ): (ArrayBuffer[Color], BitVector) = {
      def iterInsideBlock(restBits : BitRemainder,
                          lzwArray : ArrayBuffer[Color],
                          codeSize : Int,
                          prevCode : Int,
                          curBlockSize : Int
                         ): (ArrayBuffer[Color], BitRemainder, Int, Int) =
        if (curBlockSize >= blockSize * 8)
          (lzwArray, restBits, prevCode, codeSize)
        else {
          val code = splitMuchBits(codeSize, restBits)

          if (code == clearCode) {
            codeTable.reinit
            val firstCode = splitMuchBits(LZWcodeSize, restBits)
            iterInsideBlock(restBits, lzwArray :+ codeTable(firstCode).head,
              LZWcodeSize, firstCode,
              curBlockSize + codeSize + LZWcodeSize)
          } else if (code == EOIcode) {
            (lzwArray, restBits, prevCode, codeSize)
          } else if (codeTable.isDefinedAt(code)) {
            codeTable.add(codeTable(prevCode) :+ codeTable(code).head)
            iterInsideBlock(restBits, lzwArray ++ codeTable(code),
              codeSize + (if (codeTable.exceeds(codeSize)) 1 else 0),
              code, curBlockSize + codeSize)
          } else {
            val K = codeTable(prevCode).head
            codeTable.add(codeTable(prevCode) :+ K)
            iterInsideBlock(restBits, lzwArray ++ (codeTable(prevCode) :+ K),
              codeSize + (if (codeTable.exceeds(codeSize)) 1 else 0),
              code, curBlockSize + codeSize)
          }
        }

      if (blockSize == IntTerminator)
        if (leftBits.remainder == NullBitVector)
          (lzwArray, leftBits.stream)
        else {
          (ArrayBuffer(new Color(0, 0, 0)), NullBitVector)
        }
      else {
        val (newLZWarray, nextBits, lastCode, newCodeSize) =
          iterInsideBlock(leftBits, lzwArray, codeSize, prevCode, takenBitsOfFirstBlock)

        val (newBlockSize, restBits) = splitByteBits(nextBits.stream)
        iterBlock(newBlockSize, new BitRemainder(restBits, NullBitVector),
          lastCode, newLZWarray, newCodeSize)
      }
    }
    iterBlock(blockSize, bitsRemainder, firstCode,
      ArrayBuffer(codeTable(firstCode).head), codeSize, 2 * codeSize)
  }
}

case class Frame(imageInfo : ImageInfo,
                 image : Array[Array[Color]],
                 GCE : GCEAbstract) {
  def paintFrame(g : Graphics2D, dx : Int, dy : Int) {
    val start_x = imageInfo.LeftPos * dx
    val start_y = imageInfo.TopPos * dy

    for {
      x <- 0 until imageInfo.Width
      y <- 0 until imageInfo.Height
      x1 = x * dx
      y1 = y * dy
    } {
      g.setColor(image(x)(y))
      g.fillRect(start_x + x1, start_y + y1, dx, dy)
    }
  }
}

case class Animation(header : Header, frameList : List[Frame])


class GifDecoder(path : String) extends AnyDecoder {
  private def realTableSizeInBits(size : Int): Int =
    (2 << size) * 3 * 8

  @annotation.tailrec
  private def skipAEB(bits : BitVector): BitVector = {
    val (byte, rest) = splitByteBits(bits)
    if (byte == IntTerminator) rest else skipAEB(rest.drop(8 * byte))
  }


  val codecHeader = (constant(hex"474946383961".bits)  //GIF89a
    :: uint16L
    :: uint16L
    :: uintL(1)
    :: uintL(3)
    :: uintL(1)
    :: uintL(3)
    :: uint8L
    :: uint8L
    ).as[GIFHeader]

  val codecGCEB = (constant(hex"21".bits)
    :: constant(hex"f9".bits)
    :: constant(hex"04".bits)
    :: uintL(3)
    :: uintL(3)
    :: uintL(1)
    :: uintL(1)
    :: uint16L
    :: uint8L
    :: ConstTerminator
    ).as[GCEBlock]

  val codecImageInfo = (constant(hex"2c".bits) // Image Separator
    :: uint16L
    :: uint16L
    :: uint16L
    :: uint16L
    :: uintL(1)
    :: uintL(1)
    :: uintL(1)
    :: uintL(2)
    :: uintL(3)
    ).as[ImageInfo]

  val codecAEB = constant(hex"21ff0b".bits)


  val byteArray = Files.readAllBytes(Paths.get(path))
  var bitVector = BitVector(byteArray)


  private def parseHeader(bits : BitVector): (GIFHeader, GCT, BitVector) = {
    val DecodeResult(decodedHeader, restVector) =
      codecHeader.decode(bitVector).require

    val (gctBitsVector, blocksVector) =
      if (decodedHeader.GCTF == 1)
        restVector.splitAt(realTableSizeInBits(decodedHeader.GCTsize))
      else (NullBitVector, restVector)

    (decodedHeader, new GCT(gctBitsVector), blocksVector)
  }

  val (decodedHeader, gcTable, butHeadBits) = parseHeader(bitVector)


  @annotation.tailrec
  private def parseAEB(rawBits : BitVector): BitVector = {
    val aebDecResult = codecAEB.decode(rawBits)

    if (aebDecResult.isFailure) rawBits
    else {
      parseAEB(skipAEB(aebDecResult.require.remainder.drop(8 * 11))) // drops extra
    }
  }

  private def parseGCEB(rawBits : BitVector): (GCEAbstract, BitVector) = {
    val gcebDecResult = codecGCEB.decode(rawBits)

    val DecodeResult(decGCEB, butGCEbits) =
      if (gcebDecResult.isFailure)
        DecodeResult(EmptyGCEBlock, rawBits)
      else gcebDecResult.require

    (decGCEB, butGCEbits)
  }

  private def parseImageInfo(rawBits : BitVector): (ImageInfo, ColorTable, BitVector) = {
    val DecodeResult(decImageInfo, butImageBits) =
      codecImageInfo.decode(rawBits).require

    if (decImageInfo.LCTF == 1) {
      val (lctBitsVector, butLCTbits) =
        butImageBits.splitAt(realTableSizeInBits(decImageInfo.LCTsize))
      (decImageInfo, new LCT(lctBitsVector), butLCTbits)
    } else (decImageInfo, gcTable, butImageBits)
  }

  @annotation.tailrec
  private def parseRest(rawBits : BitVector,
                        frameList : List[Frame] = Nil): List[Frame] =
    if (rawBits equals Trailer)
      frameList.reverse
    else {
      val bits = parseAEB(rawBits)

      val (decGCEB, butGCEbits) = parseGCEB(bits)

      val (decImageInfo, colorTable, butImageInfoBits) = parseImageInfo(butGCEbits)

      val (lzwArray, restBits) = LZWdecoder.parseLZWtable(butImageInfoBits, colorTable)
      val pixelTable = lzwArray.toArray.grouped(decImageInfo.Width).to[Array].transpose

      parseRest(restBits, Frame(decImageInfo, pixelTable, decGCEB) :: frameList)
    }

  val Animat = Animation(decodedHeader.toHeader(gcTable), parseRest(butHeadBits))
}


object renderer extends SimpleSwingApplication {
  println(System.getProperty("user.dir"))
  val d = new GifDecoder("src/main/resources/file.gif")
  val scale = 20

  val anim = d.Animat
  val width = anim.header.Width
  val height = anim.header.Height

  def top = new MainFrame {
    var frame = anim.frameList
    Timer(frame.head.GCE.DelayTime * 10) {
      if (frame == Nil) frame = anim.frameList
      contents = new DataPanel(anim.header.Width, anim.header.Height,
        frame.head, scale, anim.header.BGColor) {
        preferredSize = new Dimension(width * scale, height * scale)
      }
      frame = frame.tail
    }
  }
}

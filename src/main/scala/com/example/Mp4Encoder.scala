package com.example

import java.awt.image.BufferedImage
import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util
import javax.imageio.ImageIO

import org.jcodec.codecs.h264.{H264Encoder, H264Utils}
import org.jcodec.common.NIOUtils
import org.jcodec.common.model.{ColorSpace, Picture, TapeTimecode}
import org.jcodec.containers.mp4.{Brand, MP4Packet, TrackType}
import org.jcodec.containers.mp4.muxer.MP4Muxer
import org.jcodec.scale.{AWTUtil, ColorUtil}

import scala.concurrent.duration._
import scala.language.postfixOps
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.std.option._
import scalaz.concurrent.Task

object Mp4Encoder extends SequenceEncoder {

  override def encode[A](frames: Seq[A]) = Task [SequenceEncoderError \/ Mp4Video] {
    if (frames.isEmpty) -\/(generalFailure("Must supply a non empty sequence of frames"))
    else {
      decodeErrorOrImages(frames) match { // [(GeneralFailure|BadlyFormedImage) \/ Seq[BufferedImage]]
        case -\/(error: SequenceEncoderError) => -\/(error)
        case \/-(images: Seq[BufferedImage]) => encodeImages(images)
      }
    }
  }

  private def generalFailure(msg: String) = GeneralFailure(new UnsupportedOperationException(msg))

  private def decodeErrorOrImages[A](frames: Seq[A]): \/[SequenceEncoderError, Seq[BufferedImage]] = {
    val badlyOrImage =
      frames map {
        case file: File => imageOrBadly(file) // [BadlyFormedImage \/ BufferedImage]
        case _ => -\/(generalFailure("Unsupported input data type"))
      }

    badlyOrImage collectFirst { case -\/(e: SequenceEncoderError) => e } toLeftDisjunction {
      badlyOrImage collect { case \/-(bi: BufferedImage) => bi }
    }
  }

  private def imageOrBadly(f: File): \/[BadlyFormedImage, BufferedImage] = {
    val bi = ImageIO.read(f)
    if (bi == null)
      -\/(BadlyFormedImage(Files.readAllBytes(f.toPath)))
    else
      \/-(bi)
  }

  private def encodeImages(images: Seq[BufferedImage]): \/[InvalidFrameDimensions, Mp4Video] = {
    val dimensions = images map (getDimensions)
    val invalid = dimensions filter (_ != dimensions.head)
    if (invalid.nonEmpty)
      -\/(InvalidFrameDimensions(invalid.head))
    else
      \/-(encodeValid(images))
  }

  private def getDimensions(bi: BufferedImage) = Dimensions(Width(bi.getWidth), Height(bi.getHeight))

  private def encodeValid(images: Seq[BufferedImage]): Mp4Video = {
    val dimensions = getDimensions(images.head)
    Mp4Video(
      encodeMp4(images),
      Dimensions(dimensions.width, dimensions.height),
      images.size seconds
    )
  }

  private def encodeMp4(images: Seq[BufferedImage]): ByteBuffer = {
    val tmp = createMp4File()
    val enc = new OneFPSEncoder(tmp)
    images foreach (i => enc.encodeImage(i))
    enc.finish()
    readFileIntoBuffer(tmp)
  }

  private def createMp4File(): File = {
    val file = Files.createTempFile(System.nanoTime().toString, ".mp4").toFile
    file.deleteOnExit()
    file
  }

  private def readFileIntoBuffer(file: File): ByteBuffer = {
    val fileChannel = new RandomAccessFile(file.getAbsolutePath, "r").getChannel
    try {
      val buffer = ByteBuffer.allocate(fileChannel.size().toInt)
      fileChannel.read(buffer)
      buffer.flip
      buffer
    } finally
      fileChannel.close()
  }
}

private class OneFPSEncoder(val out: File) {

  private val ch = NIOUtils.writableFileChannel(out)
  private val encoder = new H264Encoder
  private val colorSpaces = this.encoder.getSupportedColorSpaces
  private val transform = ColorUtil.getTransform(ColorSpace.RGB, this.colorSpaces(0))
  private val spsList = new util.ArrayList[ByteBuffer]
  private val ppsList = new util.ArrayList[ByteBuffer]
  private val muxer = new MP4Muxer(this.ch, Brand.MP4)
  private val outTrack = this.muxer.addTrack(TrackType.VIDEO, 1)
  private val _out = ByteBuffer.allocate(12441600) // 1920 * 1080 * 6

  private var frameNo = 0

  def encodeImage(bi: BufferedImage) {
    val pic = AWTUtil.fromBufferedImage(bi)
    val toEncode = Picture.create(
      pic.getWidth,
      pic.getHeight,
      colorSpaces(0)
    )
    transform.transform(pic, toEncode)
    _out.clear
    val result = encoder.encodeFrame(toEncode, _out)
    spsList.clear()
    ppsList.clear()
    H264Utils.wipePS(result, spsList, ppsList)
    H264Utils.encodeMOVPacket(result)
    val packet = new MP4Packet(
      result,
      frameNo.toLong,
      1L,
      1L,
      frameNo.toLong,
      true,
      null.asInstanceOf[TapeTimecode],
      frameNo.toLong,
      0
    )
    outTrack.addFrame(packet)
    frameNo += 1
  }

  def finish() {
    outTrack.addSampleEntry(
      H264Utils.createMOVSampleEntry(
        spsList,
        ppsList,
        4
      )
    )
    muxer.writeHeader()
    NIOUtils.closeQuietly(ch)
  }
}


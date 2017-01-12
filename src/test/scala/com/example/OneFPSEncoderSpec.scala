package com.example

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files
import javax.imageio.ImageIO

import org.jcodec.common.NIOUtils
import org.jcodec.containers.mp4.demuxer.MP4Demuxer
import org.scalatest.{FlatSpec, Matchers}

class OneFPSEncoderSpec extends FlatSpec with Matchers {

  private val PathToImageFiles = "src/test/resources/images/validImages/"
  private val Mp4HeaderSize = 56L

  // Fixtures

  def withFile(test: File => Any) {
    val tempFile = Files.createTempFile(System.nanoTime().toString, ".mp4").toFile
    tempFile.deleteOnExit()
    test(tempFile)
  }

  def withImages(test: Array[BufferedImage] => Any) {
    val images = getBufferedImages(PathToImageFiles)
    test(images)
  }

  "A OneFPSEncoder" should
    "throw an IndexOutOfBounds exception is finished is called when no frames have been encoded" in withFile { file =>
    val enc = new OneFPSEncoder(file)
    an [IndexOutOfBoundsException] should be thrownBy {
      enc.finish()
    }
  }

  // Tests

  it should "produce a 1 second MP4 file when a single frame is encoded" in withImages { images =>
    withFile { file =>
      // Given a single image
      val enc = new OneFPSEncoder(file)
      // When encode image frame
      enc.encodeImage(images.head)
      enc.finish()
      // Then resultant mp4 file greater than the header size
      file.length() should be > Mp4HeaderSize
      val stats = extractMp4Stats(file)
      stats should equal (1, 1, 1) // 1 frame, 1 fps, 1 second
    }
  }

  it should "produce a 2 second MP4 file when a 2 frames are encoded" in withImages { images =>
    withFile { file =>
      // Given a single image
      val enc = new OneFPSEncoder(file)
      // When encode 2 image frames
      images.take(2).foreach(enc.encodeImage)
      enc.finish()
      // Then resultant mp4 file greater than the header size
      file.length() should be > Mp4HeaderSize
      val stats = extractMp4Stats(file)
      stats should equal (2, 1, 2) // 2 frames, 1 fps, 2 seconds
    }
  }

  it should "produce a 3 second MP4 file when a 3 frames are encoded" in withImages { images =>
    withFile { file =>
      // Given a single image
      val enc = new OneFPSEncoder(file)
      // When encode 2 image frames
      images.take(3).foreach(enc.encodeImage)
      enc.finish()
      // Then resultant mp4 file greater than the header size
      file.length() should be > Mp4HeaderSize
      val stats = extractMp4Stats(file)
      stats should equal (3, 1, 3) // 3 frames, 1 fps, 3 seconds
    }
  }

  it should "produce a 19 second MP4 file when 19 frames are encoded" in withImages { images =>
    withFile { file =>
      // Given a single image
      val enc = new OneFPSEncoder(file)
      // When encode 2 image frames
      images.foreach(enc.encodeImage)
      enc.finish()
      // Then resultant mp4 file greater than the header size
      file.length() should be > Mp4HeaderSize
      val stats = extractMp4Stats(file)
      stats should equal (19, 1, 19) // 19 frames, 1 fps, 19 seconds
    }
  }

  // Helper methods

  private def getBufferedImages(dir: String): Array[BufferedImage] = {
    for {
      file <- new File(dir).listFiles
      if !file.isHidden && file.getName.endsWith(".jpg")
      image = ImageIO.read(file)
    } yield image
  }

  private def extractMp4Stats(file: File): (Long, Long, Long) = {
    val source = NIOUtils.readableFileChannel(file)
    try {
      val track = new MP4Demuxer(source).getVideoTrack
      val frameCount = track.getFrameCount
      val timescale = track.getTimescale
      (frameCount, timescale, frameCount * timescale) // frame count, timescale (frmaes/sec), duration (seconds)
    } finally {
      source.close()
    }
  }
}


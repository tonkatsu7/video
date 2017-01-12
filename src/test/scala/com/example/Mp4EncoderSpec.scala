package com.example

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.{DisjunctionMatchers, DisjunctionValues}

import scala.concurrent.duration._
import scala.language.postfixOps

class Mp4EncoderSpec extends FlatSpec with Matchers with DisjunctionMatchers with DisjunctionValues {

  private val PathToImageFiles = "src/test/resources/images/validImages/"
  private val PathToInvalidDimensionsFiles = "src/test/resources/images/invalidDimensions/"
  private val PathToBadlyFormedImageFiles = "src/test/resources/images/badlyFormed/"

  // Fixtures

  def withFiles(test: Array[File] => Any, dir: String) {
    val images = getImageFiles(dir)
    test(images)
  }

  val withImageFiles = withFiles(_: Array[File] => Any, PathToImageFiles)

  val withInvalidDimensionsFiles = withFiles(_: Array[File] => Any, PathToInvalidDimensionsFiles)

  val withBadlyFormedFiles = withFiles(_: Array[File] => Any, PathToBadlyFormedImageFiles)

  // Tests

  "An Mp4Encoder" should
    "return a general failure error if there are no frames passed in" in {
    // Given an empty sequence of image files
    val noFrames = List[File]()
    // When encode
    val result = Mp4Encoder.encode(noFrames).unsafePerformSync
    // Then general failure error returned
    result should be (left)
    result.leftValue shouldBe a [GeneralFailure]
    val failure = result.leftValue.asInstanceOf[GeneralFailure]
    failure.error shouldBe a [UnsupportedOperationException]
    failure.error.getMessage should equal ("Must supply a non empty sequence of frames")
  }

  it should "encode a single frame into a 1 second MP4 clip" in withImageFiles { images =>
    // Given a single sequence of images files
    // When encode
    val result = Mp4Encoder.encode(images.take(1)).unsafePerformSync
    // Then video of 1 second
    result should be (right)
    result.value shouldBe a [Mp4Video]
    val success = result.value.asInstanceOf[Mp4Video]
    success.content.array().length should be (178805)
    success.dimensions should be (Dimensions(Width(1229), Height(768)))
    success.length should be (1 second)
  }

  it should "encode 2 frames into a 2 second MP4 clip" in withImageFiles { images =>
    // Given a sequence of 2 image files
    // When encode
    val result = Mp4Encoder.encode(images.take(2)).unsafePerformSync
    // Then video of 2 seconds
    result should be (right)
    result.value shouldBe a [Mp4Video]
    val success = result.value.asInstanceOf[Mp4Video]
    success.content.array().length should be (356593)
    success.dimensions should be (Dimensions(Width(1229), Height(768)))
    success.length should be (2 seconds)
  }

  it should "encode 3 frames into a 3 second MP4 clip" in withImageFiles { images =>
    // Given a sequence of 2 image files
    // When encode
    val result = Mp4Encoder.encode(images.take(3)).unsafePerformSync
    // Then video of 3 seconds
    result should be (right)
    result.value shouldBe a [Mp4Video]
    val success = result.value.asInstanceOf[Mp4Video]
    success.content.array().length should be (534640)
    success.dimensions should be (Dimensions(Width(1229), Height(768)))
    success.length should be (3 seconds)
  }

  it should "encode 19 frames into a 19 second MP4 clip" in withImageFiles { images =>
    // Given a sequence of 2 image files
    // When encode
    val result = Mp4Encoder.encode(images).unsafePerformSync
    // Then video of N seconds
    result should be (right)
    result.value shouldBe a [Mp4Video]
    val success = result.value.asInstanceOf[Mp4Video]
    success.content.array().length should be (3390786)
    success.dimensions should be (Dimensions(Width(1229), Height(768)))
    success.length should be (images.length seconds)
  }

  it should "return an invalid frame dimension error if a frame has inconsistent dimensions" in withInvalidDimensionsFiles { images =>
    // Given a sequence of image files with varying frame dimensions
    // When encode
    val result = Mp4Encoder.encode(images).unsafePerformSync
    // Then invalid frame dimensions error returned containing the first inconsistent frame dimensions
    result should be (left)
    result.leftValue shouldBe a [InvalidFrameDimensions]
    val invalid = result.leftValue.asInstanceOf[InvalidFrameDimensions]
    invalid.dimensions should be (Dimensions(Width(800), Height(533)))
  }

  it should "return a badly formed image error if a frame is a badly formed image" in withBadlyFormedFiles { images =>
    // Given a sequence of image files with a badly formed image
    // When encode
    val result = Mp4Encoder.encode(images).unsafePerformSync
    // Then badly formed image error returned with the data of teh badly formed image
    result should be (left)
    result.leftValue shouldBe a [BadlyFormedImage]
    val badly = result.leftValue.asInstanceOf[BadlyFormedImage]
    badly.data.length should be (395)
  }

  it should "return a general failure error if there are non java.io.File types passed in" in {
    // Given an empty sequence of image files
    val nonFrames = List[String]("one", "two", "three")
    // When encode
    val result = Mp4Encoder.encode(nonFrames).unsafePerformSync
    // Then general failure error returned
    result should be (left)
    result.leftValue shouldBe a [GeneralFailure]
    val failure = result.leftValue.asInstanceOf[GeneralFailure]
    failure.error shouldBe a [UnsupportedOperationException]
    failure.error.getMessage should equal ("Unsupported input data type")
  }

  // Helper methods

  private def getImageFiles(dir: String): Array[File] = {
    for {
      file <- new File(dir).listFiles()
      if !file.isHidden && file.getName.endsWith(".jpg")
    } yield file
  }
}

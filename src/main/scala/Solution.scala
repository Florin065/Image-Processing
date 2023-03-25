import util.Pixel
import util.Util
import util.Util.{GrayscaleImage, getNeighbors, toGrayScale}

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val lines = (image.mkString split '\n').toList
    val header = lines take 3
    val pixels = lines drop 3 flatMap (
      _.trim.split("\\s+").map(_.toInt)
      )

    val dimensions = header(1)
      .split(' ')
      .map(_.toInt) // width and height

    (pixels grouped 3)
      .toList
      .map(chunk => Pixel(chunk.head, chunk(1), chunk(2)))
      .grouped(dimensions.head)
      .toList
  }

  def toStringPPM(image: Image): List[Char] = {
    ("P3\n" // PPM format
      + s"${image.head.length}" // width
      + " "
      + s"${image.length}\n" // height
      + "255\n"
      :: image
      .flatten
      .map(
        p => s"${p.red} ${p.green} ${p.blue}\n"
      )
      ).mkString.toList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image =
    image1 ++ image2  // concatenate the rows

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image =
    for (tuple <- image1 zip image2) yield tuple._1 ++ tuple._2 // zip the rows


  // ex 3
  def rotate(image: Image, degrees: Integer): Image =
    val norm = (degrees % 360) / 90          // normalize the degrees
    norm match {
      case 1 => image.transpose.reverse      //  90 degrees
      case 2 => image.reverse.map(_.reverse) // 180 degrees
      case 3 => image.reverse.transpose      // 270 degrees
      case 0 => image                        // 360 degrees
    }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def edgeDetection(image: Image, threshold: Double): Image = {
    // convert to grayscale
    val grayScaleImage = image.map(_.map(toGrayScale))

    // apply convolution to the grayscale image
    val blur = applyConvolution(grayScaleImage, gaussianBlurKernel)

    // apply convolution to the gradient images
    val Mx = applyConvolution(blur, Gx)
    val My = applyConvolution(blur, Gy)

    Mx zip My map { case (rowX, rowY) =>           // for each row
      rowX zip rowY map { case (x, y) =>           // for each pixel
        val magnitude = x.abs + y.abs              // magnitude of the gradient
        if (magnitude < threshold) Pixel(0, 0, 0)  // black
        else Pixel(255, 255, 255)                  // white
      }
    }
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbors = getNeighbors(image, kernel.length / 2)  // get the neighbors of each pixel

    for (neighbor <- neighbors) yield {                     // for each neighbor
      for (pixel <- neighbor) yield {                       // for each pixel
        (pixel.flatten zip kernel.flatten)                  // zip the pixels and the weights
          .map { case (pixel, weight) => pixel * weight }   // multiply the pixels and the weights
          .sum                                              // sum the result
      }
    }
  }

    // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    ???
  }
}
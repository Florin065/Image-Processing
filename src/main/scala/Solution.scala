import util.Pixel
import util.Util
import util.Util.{GrayscaleImage, getNeighbors, toGrayScale}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val lines = (image.mkString split '\n').toList          // split the image into lines
    val header = lines take 3                               // get the header
    val pixels = lines drop 3 flatMap (                     // get the pixels
      _.trim.split("\\s+").map(_.toInt)              // split the line into pixels
      )

    val dimensions = header(1)                              // get the dimensions
      .split(' ')                                           // split the line into width and height
      .map(_.toInt)                                         // width and height as integers

    (pixels grouped 3)                                      // group the pixels into chunks of 3
      .toList                                               // convert to list
      .map(chunk => Pixel(chunk.head, chunk(1), chunk(2)))  // convert to pixels
      .grouped(dimensions.head)                             // group the pixels into rows
      .toList                                               // convert to list
  }

  def toStringPPM(image: Image): List[Char] = {
    ("P3\n"                                                 // PPM format
      + s"${image.head.length}"                             // width
      + " "                                                 // space
      + s"${image.length}\n"                                // height
      + "255\n"                                             // max value
      :: image                                              // append the image
      .flatten                                              // flatten the image
      .map(                                                 // for each pixel
        p => s"${p.red} ${p.green} ${p.blue}\n"             // convert to string
      )
      ).mkString.toList                                     // join the lines and convert to list
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image =
    image1 ++ image2 // concatenate the rows

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image =
    for (tuple <- image1 zip image2) yield tuple._1 ++ tuple._2 // zip the rows

  // ex 3
  def rotate(image: Image, degrees: Integer): Image =
    val norm = (degrees % 360) / 90           // normalize the degrees
    norm match {
      case 1 => image.transpose.reverse       //  90 degrees
      case 2 => image.reverse.map(_.reverse)  // 180 degrees
      case 3 => image.reverse.transpose       // 270 degrees
      case 0 => image                         // 360 degrees
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
    val grayScaleImage = image map (_.map(toGrayScale))

    // apply convolution to the grayscale image
    val blur = applyConvolution(grayScaleImage, gaussianBlurKernel)

    // apply convolution to the gradient images
    val Mx = applyConvolution(blur, Gx)
    val My = applyConvolution(blur, Gy)

    Mx zip My map { (rowX, rowY) =>                 // for each row
      rowX zip rowY map { (x, y) =>                 // for each pixel
        val magnitude = x.abs + y.abs               // magnitude of the gradient
        if (magnitude < threshold) Pixel(0, 0, 0)   // black
        else Pixel(255, 255, 255)                   // white
      }
    }
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbors = getNeighbors(image, kernel.length / 2) // get the neighbors of each pixel

    for (neighbor <- neighbors) yield {                    // for each neighbor
      for (pixel <- neighbor) yield {                      // for each pixel
        (pixel.flatten zip kernel.flatten)                 // zip the pixels and the weights
          .map { (pixel, weight) => pixel * weight }       // multiply the pixels and the weights
          .sum                                             // sum the result
      }
    }
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def gen(prev: List[Int]): List[List[Int]] = {
      if (prev.length > size) Nil               // stop condition
      else prev :: gen((0 :: prev)              // prepend 0
        zip prev.appended(0)                    // append 0
        map (pair => (pair._1 + pair._2) % m))  // sum the elements and apply modulo
    }

    gen(List(1))                                // generate the pascal triangle
      .map(                                     // for each row
        _.map(funct(_))                         // apply the function
          .padTo(size, Pixel(0, 0, 0))          // pad the row with black pixels
      )
  }
}
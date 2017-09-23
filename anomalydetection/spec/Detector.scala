package anomalydetection.spec

import anomalydetection.util.UtilFunctions._

abstract class Detector(val dist:Array[Array[Double]],val k:Int,val reg:Double){
  // Abstract algorithm
  def detect()=fit(dist,k,reg){penalty}

  private def fit(matrix: Array[Array[Double]], k: Int, reg: Double)
         (penalty: (Array[Array[Double]], Double) => Array[Double]):(Array[Int], Array[Int], Array[Double]) = {

    def run(G: Array[Int], M: Array[Int], W: Array[Double]): (Array[Int], Array[Int], Array[Double]) = {
      val newG = computeClusters(matrix, M)
      val newM = computeMedoids(matrix, newG)
      val newW = computeWeights(matrix, newG, newM, reg){penalty}

      if (W.sameElements(newW))
        (newG, newM, newW)
      else
        run(newG, newM, newW)
    }

    run(getArray(-1,matrix.size), generateInitialMedoids(k, matrix.size), getArray(Double.NegativeInfinity,matrix.size))

  }
  // core components of algorithm
  private def computeClusters(matrix: Array[Array[Double]], medoids: Array[Int]): Array[Int] = {
    getPartialMatrix(matrix, medoids).foldLeft(Array[Int]())((clusters, current) => clusters :+ {
      current.foldLeft(-1, Double.MaxValue, 0) {
        case ((minIndex, minValue, currentIndex), currentValue) =>
          if (currentValue < minValue) (currentIndex, currentValue, currentIndex + 1)
          else (minIndex, minValue, currentIndex + 1)
      }._1
    }

    )


  }
  private def computeMedoids(matrix: Array[Array[Double]], clusters: Array[Int]): Array[Int] = {

    clusters.toSet.foldLeft(Array[Int]())((r, cls) => r :+ {
      getSetOfIndex(clusters, cls)(
        indexMin(
          getSquareMatrix(matrix, getSetOfIndex(clusters, cls)).foldLeft(
            Array[Double]())(
            (r, c) => r :+ sumArray(c))
        )
      )

    })
  }
  private def computeWeights(distanceMatrix: Array[Array[Double]], clusters: Array[Int], medoids: Array[Int], reg: Double)(penalty: (Array[Array[Double]], Double) => Array[Double]): Array[Double] = {
    penalty(distanceMatrix.map(row => medoids.map(i => row(i))), reg)
  }


  protected def penalty(DI:Array[Array[Double]], reg:Double): Array[Double]
}
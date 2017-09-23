package anomalydetection.spec

import anomalydetection.util.UtilFunctions._

/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */

trait L2GAD{
  def l2gad(DI:Array[Array[Double]],beta:Double):(Array[Array[Double]],Array[Array[Double]])=  {
    val DI_adjusted= for (d<-DI)
      yield
        { d map (_+Double.MinPositiveValue) }

    val clusters=(0 until DI_adjusted(0).length).toArray

    val weights=(0 until DI_adjusted.length).foldLeft(Array[Array[Double]]())((acc, current) =>

      acc :+ {

        (0 until DI_adjusted(0).length).foldLeft(Array[Double]())((weight, cluster_id) =>

          weight :+ {
            val z = getSetOfIndex(clusters ,cluster_id).foldLeft(0.0) {
              case (accumulator, now) =>
                accumulator +
                  Math.pow( DI_adjusted(current)(cluster_id),1/(beta-1)) / Math.pow(DI_adjusted(now)(cluster_id),1/(beta-1))            }

            Math.pow(z, -1)
          }
        )
      }
    )
    (weights,DI_adjusted)
  }
}


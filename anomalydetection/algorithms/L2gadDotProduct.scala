package anomalydetection.algorithms

import anomalydetection.spec.{Detector,L2GAD}
import anomalydetection.util.UtilFunctions.dotProduct

/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */

class L2gadDotProduct  (dist:Array[Array[Double]],k:Int,beta:Double) extends  Detector(dist,k,beta) with L2GAD{

  protected def penalty (DI:Array[Array[Double]],beta:Double):Array[Double]={
    val (weights,distancesAdjusted)= l2gad(DI,beta)
    weights zip distancesAdjusted map (elt=> dotProduct(elt._1,elt._2))
  }
}


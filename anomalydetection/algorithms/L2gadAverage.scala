package anomalydetection.algorithms

import anomalydetection.spec.{Detector,L2GAD}

/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */

 class L2gadAverage (dist:Array[Array[Double]],k:Int, beta:Double) extends  Detector(dist,k,beta) with L2GAD{

  protected def penalty (DI:Array[Array[Double]],beta:Double):Array[Double]={
    val (weights,_)= l2gad(DI,beta)
    weights map ( finalWeight=> finalWeight.sum/finalWeight.length)
  }

}

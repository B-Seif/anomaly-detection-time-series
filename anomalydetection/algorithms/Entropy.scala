package anomalydetection.algorithms

import anomalydetection.spec.Detector

/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */
class Entropy (dist:Array[Array[Double]], k:Int,lambda:Double) extends  Detector(dist,k,lambda){


 // here
  protected override  def penalty(DI:Array[Array[Double]],reg:Double):Array[Double]= {
    val closest=  DI.foldLeft( Array[Double]())((r,c) => r:+c.min )
    closest.foldLeft(Array[Double]()){
      case (tab, current) => tab :+ {
        Math.exp(- current / reg) / (closest.foldLeft(0.0)((r, c) => r + Math.exp(-c / reg)))
      }
    }
  }

}
package anomalydetection.algorithms

import anomalydetection.spec.Detector

/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */

class Ridge (dist:Array[Array[Double]],k:Int,alpha:Double) extends  Detector(dist,k,alpha){


  protected def penalty(DI:Array[Array[Double]],lambda :Double):Array[Double]={
    val closest= DI.foldLeft(Array[Double]())((r,c)=> r:+c.min )
    closest.foldLeft(Array[Double]()){
      case(tab,current)=>  tab:+ {
        ((2*lambda)+closest.sum-(closest.size*current)) / (2*lambda*closest.sum)
      }
    }
  }


}
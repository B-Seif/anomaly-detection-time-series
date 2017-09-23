/**
  * Created by Seif-Eddine Benkabou on 18/08/2017.
  */
package anomalydetection.spec

case class Output( clusterId:Int,  medoidId:Int,  weight:Double){
  override def toString=" | cluster ID = "+clusterId+" medoids ID = "+medoidId+" with weight = "+weight
}
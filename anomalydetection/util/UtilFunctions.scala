/**
  * Created by Seif-Eddine Benkabou on 28/08/2017.
  */
package anomalydetection.util

object UtilFunctions{
  // useful methods used by the algorithm (helper functions...)
  def getSetOfIndex(clusters: Array[Int], cls: Int): Array[Int] = {
    clusters.foldLeft(Array[Int](), 0) {
      case ((tab, currentIndex), currentValue) => if (currentValue == cls) (tab :+ currentIndex, currentIndex + 1)
      else (tab, currentIndex + 1)
    }._1
  }
  def indexMin(a: Array[Double]): Int = {
    a.foldLeft(-1, Double.MaxValue, 0) {
      case ((minIndex, minValue, currentIndex), currentValue) =>
        if (currentValue < minValue) (currentIndex, currentValue, currentIndex + 1)
        else (minIndex, minValue, currentIndex + 1)

    }._1
  }
  def sumArray(a: Array[Double]): Double = {
    a.foldLeft(0.0)(_ + _)
  }
  def getSquareMatrix(matrix: Array[Array[Double]], setOfIndex: Array[Int]): Array[Array[Double]] = {
    setOfIndex.map(i => matrix(i)).map(row => setOfIndex.map(j => row(j)))
  }
  def getPartialMatrix(matrix: Array[Array[Double]], medoids: Array[Int]): Array[Array[Double]] = {
    matrix.map(row => medoids.map(i => row(i)))
  }
  def getValue(tab: Array[Int], value: Int, maxValue: Int): Int = {
    if (tab.contains(value) == false)
      value
    else
      getValue(tab, scala.util.Random.nextInt(maxValue), maxValue)
  }
  def generateInitialMedoids(k:Int,n:Int):Array[Int]= {

    require (n>=k)

    (1 to k).foldLeft(Array[Int]())((acc,_) =>
      acc :+{
        getValue(acc, scala.util.Random.nextInt(n),n)
      }
    )
  }
  def getArray[A: scala.reflect.ClassTag](value: A,ntimes:Int) = {
    (Array.fill(ntimes)(value))
  }
  def showMatrix(weight:Array[Array[Double]]){
    weight map( row=>{
      row foreach  (elt=>print(elt +"   "))
      println()
    }
      )
  }
  def dotProduct(x:Array[Double],y:Array[Double])={
    x.foldLeft(0.0,0){
      case ((acc,index),current)=> (acc+ (current*y(index)) ,index+1)
    }._1

  }
  def dtw(s:Array[Double], t:Array[Double], w:Double=Double.PositiveInfinity):Double ={
    // adjust window length
    val window=Math.max(w,Math.abs( s.length - t.length))
    // cache matrix init to Inf
    val D=Array.fill(s.length+1,t.length+1)(Double.PositiveInfinity)
    D(0)(0)=0.0
    for( i<- 0 to s.length-1 )
      for ( j <- Math.max(i-window,0).toInt  to   Math.min(i+window,t.length-1).toInt)
        D(i+1)(j+1)=
          Math.sqrt(Math.pow(s(i)-t(j),2)) +Array( D(i)(j+1) , D(i+1)(j), D(i)(j) ).min
    D(s.length)(t.length)
  }
}

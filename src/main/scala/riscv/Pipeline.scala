package riscv

import spinal.core._

import scala.collection.mutable
import scala.reflect.ClassTag

trait Pipeline {
  private val plugins = mutable.ArrayBuffer[Plugin[this.type]]()

  def config: Config
  def data: StandardPipelineData

  // NOTE This is deliberately not just called "component" because that causes a
  // (silent) name-clash with one of the traits implemented by Component.
  def pipelineComponent: Component

  def stages: Seq[Stage]
  def fetchStage: Stage
  def passThroughStage: Stage // TODO: this only makes sense for the dynamic pipeline?
  def retirementStage: Stage

  def addPlugin(plugin: Plugin[this.type]): Unit = {
    plugins += plugin
  }

  def addPlugins(plugins: Seq[Plugin[this.type]]): Unit = {
    this.plugins ++= plugins
  }

  final def initBuild(): Unit = {
    pipelineComponent.rework {
      init()
    }

    plugins.foreach(_.setPipeline(this))
  }

  final def setupPlugins(): Unit = {
    plugins.foreach(_.setup())
  }

  final def buildPlugins(): Unit = {
    plugins.foreach(_.build())
  }

  final def finishBuild(): Unit = {
    pipelineComponent.rework {
      connectStages()
    }

    plugins.foreach(_.finish())
  }

  def build(): Unit = {
    initBuild()
    setupPlugins()
    buildPlugins()
    finishBuild()
  }

  protected def init(): Unit
  protected def connectStages(): Unit

  def serviceOption[T](implicit tag: ClassTag[T]): Option[T] = {
    val services = plugins.filter(_ match {
      case _: T => true
      case _ => false
    })

    assert(services.length <= 1)
    services.headOption.map(_.asInstanceOf[T])
  }

  def service[T](implicit tag: ClassTag[T]): T = {
    serviceOption[T].get
  }

  def hasService[T](implicit tag: ClassTag[T]): Boolean = {
    serviceOption[T].isDefined
  }

  def getImplementedExtensions: Seq[Extension] = {
    Extension(config.baseIsa) +: plugins.flatMap(_.getImplementedExtensions)
  }
}

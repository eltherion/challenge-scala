package pl.datart.csvtojson.service

import nl.grons.metrics4.scala.DefaultInstrumented
import pl.datart.csvtojson.model._

import java.time.temporal.ChronoUnit._
import java.util.Date

trait StatsComposer {
  def createReport(task: Task): TaskStats
}

object StatsComposerImpl extends StatsComposer with DefaultInstrumented {
  override def createReport(task: Task): TaskStats = {
    TaskStats(
      linesProcessed = metrics.counter(task.taskId.taskId).count,
      avgLinesPerSec = getAvgLinesPerSec(task),
      state = task.state,
      result = resultUri(task)
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def getAvgLinesPerSec(task: Task): Long = {
    val countedLines = metrics.counter(task.taskId.taskId).count
    task.state match {
      case TaskState.Scheduled | TaskState.Canceled | TaskState.Failed =>
        0L
      case TaskState.Running                                           =>
        task.startTime
          .fold[Long](0L) { start =>
            val timeDiff = start.toInstant.until(new Date().toInstant, SECONDS)
            if (timeDiff != 0L) {
              countedLines / timeDiff
            } else {
              0L
            }
          }
      case TaskState.Done                                              =>
        (task.startTime, task.endTime) match {
          case (Some(start), Some(end)) =>
            val timeDiff = start.toInstant.until(end.toInstant, SECONDS)
            if (timeDiff != 0L) {
              countedLines / timeDiff
            } else {
              0L
            }
          case _                        =>
            0L
        }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def resultUri(task: Task): Option[String] = {
    if (task.state == TaskState.Done) {
      Option(s"/file/${task.taskId.taskId}")
    } else {
      Option.empty[String]
    }
  }
}

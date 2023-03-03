package todo

import cats.implicits.*

import scala.collection.mutable
import todo.data.*
import todo.data.State.Completed

/**
 * The InMemoryModel is a Model that stores all the tasks in RAM, and hence they
 * are lost when the server restarts.
 *
 * You should modify this file.
 */
object InMemoryModel extends Model:
  /* These are the tasks the application starts with. You can change these if you want. */
  val defaultTasks = List(
    Id(0) -> Task(State.completedNow, "Complete Effective Scala Week 2", None, List(Tag("programming"), Tag("scala"))),
    Id(1) -> Task(State.Active, "Complete Effective Scala Week 3", Some("Finish the todo list exercise"), List(Tag("programming"), Tag("scala"), Tag("encapsulation"), Tag("sbt"))),
    Id(2) -> Task(State.Active, "Make a sandwich", Some("Cheese and salad or ham and tomato?"), List(Tag("food"), Tag("lunch")))
  )

  /* Every Task is associated with an Id. Ids must be unique. */
  private val idGenerator = IdGenerator(Id(3))

  /* The idStore stores the associated between Ids and Tasks. We use a
   * LinkedHashMap so we can access elements in insertion order. We need to keep
   * a stable order so the UI doesn't jump around, which would be confusing to
   * the user.
   *
   * Note that this data structure is not safe to use with concurrent access.
   * This doesn't matter in this case study, but in a real situation it would be
   * a problem. In a future week we'll learn the techniques to address this. */
  private val taskById: mutable.LinkedHashMap[Id, Task] =
    mutable.LinkedHashMap.from(defaultTasks)

  def create(task: Task): Id =
    val id = idGenerator.nextId()
    taskById += id -> task
    id

  def read(id: Id): Option[Task] =
    taskById.get(id)

  def complete(id: Id): Option[Task] =
    taskById.updateWith(id)(opt => opt.map(_.copy(state = State.completedNow)))

  def update(id: Id)(f: Task => Task): Option[Task] =
    taskById.updateWith(id)(opt => opt.map(f))

  def delete(id: Id): Boolean =
    taskById.remove(id).isDefined

  def tasks: Tasks =
    Tasks(taskById)

  def tags: Tags =
    val uniqueTags = taskById.values.view.flatMap(_.tags).toSet.toList
    Tags(uniqueTags)

  def tasks(tag: Tag): Tasks =
    val taggedTasks = taskById.filter { case (_, task) => task.tags.contains(tag) }
    Tasks(taggedTasks)

  def clear(): Unit =
    taskById.clear()

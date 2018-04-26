
import java.util.*;

public class Scan {
  public enum Key {
    X,
    Y,
    Food,
    Victory,
    ChoppedIngredients,
    Reputation
  }

  public static class Interval {
    double low;
    double high;
  }

  public static class Task {
    public String name;
    public Set<Key> needs;
    public Set<Key> outcomes;
  }

  public static class TaskNode {
    public TaskNode successor;
    public Task task;
    public double cost;

    public TaskNode(Task task, double cost, TaskNode successor) {
      this.successor = successor;
      this.task = task;
      this.cost = cost;
    }
  }

  public List<TaskNode> findPredecessors(TaskNode successor) {
    // Cool, so, we're trying to find a TaskNode that could resolve one of the un-solved issues in the successor.
    // Let's just make sure those exist!

    if (successor.isComplete()) {
      throw new IllegalStateException("hmm yeah.");
    }

    List<Task> subtasks = new ArrayList<>();

    // Let's find solutions for each need and do something about them.
    double directCost = 0D;
    for (Key need : successor.task.needs) {
      if (directPrices.containsKey(need)) {
        directCost += directPrices.get(need);
      } else {
        // For each Need, we gotta find all of the Tasks that could resolve it
        for (Task task : allTasks) {
          if (task.outcomes.containsKey(need)) {
            // Cool, this task is one of the good ones, so let's make a Node from it later
            subtasks.add(task);
          }
        }
      }
    }
    successor.directCost += directCost;

    List<TaskNode> subnodes = new ArrayList<>();
    for (Task task : subtasks) {
      subnodes.add(new TaskNode(task, successor.directCost, successor));
    }

    return subnodes;

    // questions:
    // 1) why does the node need a successor? we just know that a task offers Completion, even if we don't know what for
    // 1a) cool because if we don't need a successor in a tasknode then we probably don't need mutation anymore
    // 2) if we are including the direct costs when calculating the cost of a predecessor, shouldn't we also include the other task costs? i'm confused about this.
    // 3) do we care about dependent vs independent tasks? example: i want to build a house. the house needs wood and stone. do we care how long the stone gathering will take, if all i'm planning to do is chop wood? how far do we take this logic?
  }
}

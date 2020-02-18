use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
    fn heapify(&mut self, i: usize, size: usize) -> ();
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/**
    These traits are implemented for Nodes to make them comparable
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}

/**
    You must implement the above trait for the vector type
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele);

        if self.len() > 1{

            let mut mid = (((self.len() as i32) - 2) / 2) as i32;
            let mut p = (self.len() - 1) as i32;

            while self[p as usize] < self[mid as usize] {
                self.swap(p as usize, mid as usize);
                p = mid as i32;
                mid = ((p - 1) / 2) as i32;
            }  
        }     
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/

    fn heapify(&mut self, i: usize, size: usize) -> () {

        let mut l = (2 * i) + 1;
        let mut r = (2 * i) + 2;
        let mut s = i; 
        if l < size && self[l] < self[i]{
            s = l;

        }

        if r < size && self[r] < self[s]{
            s = r;

        }

        if s != i {
            self.swap(i, s);
            self.heapify(s, size);
        }

    } 

    fn dequeue(&mut self) -> Option<T> {
        let mut heapsize = self.len();
        if heapsize <= 0 {
            return None;
        }

        let mut temp = Some(self.swap_remove(0));
        if heapsize == 1 {
            return temp;
        }
        heapsize -= 1;
        self.heapify(0, heapsize);
        return temp;  

    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0{ 
            None
        }else{
            Some(&self[0])
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let (x1, y1) = p1;
    let (x2, y2) = p2;
    (x1 - x2).abs() + (y1 - y2).abs()
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut dist = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut temp: i32 = 0;
    let mut min: i32 = 0;

    for (k1, (a1, b1)) in enemies {
        min = 10000;
        for (k2, (a2, b2)) in allies{
            temp = distance((*a1, *b1), (*a2, *b2));
            if temp < min {
                min = temp;
            }
            if *k2 == "Stark"{
                x = *a2;
                y = *b2;
            }
        }
        dist.insert(k1, (min, a1, b1));
    }

    for (k, (m, a, b)) in dist {
        if m == distance((*a, *b), (x, y)) {
            return (k, *a, *b)
        }

    }
    ("Stark", 1, 2)
}

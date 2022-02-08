pub struct Tree<T> {
    arena: Vec<Node<T>>
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree {
            arena: vec![]
        }
    }

    pub fn root(val: T) -> Tree<T> {
        Tree {
            arena: vec![Node {
                id: 0,
                val,
                parent: None,
                children: vec![]
            }]
        }
    }

    pub fn insert(&mut self, val: T, parent: usize) -> usize {
        let id = self.arena.len();
        self.arena.push(Node {
            id,
            val,
            parent: Some(parent),
            children: vec![]
        });

        self.arena[parent].children.push(id);

        id
    }

    pub fn get(&self, id: usize) -> Option<&Node<T>> {
        self.arena.get(id)
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut Node<T>> {
        self.arena.get_mut(id)
    }
}

pub struct Node<T> {
    id: usize,
    val: T,
    parent: Option<usize>,
    children: Vec<usize>,
}

use std::convert::TryInto;

use super::lossless::BitReader;

/// Rudimentary utility for reading Canonical Huffman Codes.
/// Based off https://github.com/webmproject/libwebp/blob/f2623dbe583b28f1bff5678193d00ea8c872666c/src/utils/huffman.c
/// 

const MAX_ALLOWED_CODE_LENGTH: usize = 15;


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum HuffmanTreeNode {
    Branch(usize), //offset in vector to children
    Leaf(u16), //symbol stored in leaf
    Empty,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct HuffmanTree {
    pub(crate) tree: Vec<HuffmanTreeNode>,
    max_nodes: usize,
    pub(crate) num_nodes: usize,
}

impl HuffmanTree {
    fn is_full(&self) -> bool {
        self.num_nodes == self.max_nodes
    }

    fn assign_children(&mut self, node_index: usize) -> usize {
        let offset_index = self.num_nodes - node_index;
        self.tree[node_index] = HuffmanTreeNode::Branch(offset_index);
        self.num_nodes += 2;
        
        offset_index
    }

    fn init(num_leaves: usize) -> HuffmanTree {
        if num_leaves == 0 {panic!("djeiofje")};

        let max_nodes = 2 * num_leaves - 1;
        let tree = vec![HuffmanTreeNode::Empty; max_nodes];
        let num_nodes = 1;
        
        HuffmanTree {
            tree,
            max_nodes,
            num_nodes,
        }
    }

    fn code_lengths_to_codes(code_lengths: &Vec<u16>) -> Vec<Option<u16>> {

        let max_code_length = *code_lengths.iter().reduce(|a, b| {
            if a >= b {a} else {b}
        }).unwrap();

        if max_code_length > MAX_ALLOWED_CODE_LENGTH.try_into().unwrap() {
            panic!("aaaaaa");
        }

        let mut code_length_hist = vec![0; MAX_ALLOWED_CODE_LENGTH + 1];

        for &length in code_lengths.iter() {
            code_length_hist[usize::from(length)] += 1;
        }

        code_length_hist[0] = 0;

        let mut curr_code = 0;
        let mut next_codes = vec![None; MAX_ALLOWED_CODE_LENGTH + 1];

        for code_len in 1..=usize::from(max_code_length) {
            curr_code = (curr_code + code_length_hist[code_len - 1]) << 1;
            next_codes[code_len] = Some(curr_code);
        }

        let mut huff_codes = vec![None; code_lengths.len()];

        for symbol in 0..code_lengths.len() {
            let length = usize::from(code_lengths[symbol]);
            if length > 0 {
                huff_codes[symbol] = next_codes[length];
                if let Some(value) = next_codes[usize::from(code_lengths[symbol])].as_mut() {
                    *value += 1;
                }
            } else {
                huff_codes[symbol] = None;
            }
        }

        huff_codes
    }

    fn add_symbol(&mut self, symbol: u16, code: u16, code_length: u16) {
        let mut node_index = 0;
        let code = usize::from(code);

        let mut length = code_length;

        while length > 0 {
            length -= 1;
            if node_index >= self.max_nodes {
                panic!("jdeiofhe");
            }

            let node = self.tree[node_index];

            let offset = match node {
                HuffmanTreeNode::Empty => {
                    if self.is_full() {
                        panic!("{:?}, {:b}, tree full", self, code_length);
                    }
                    self.assign_children(node_index)
                }
                HuffmanTreeNode::Leaf(_) => panic!("leaf occupied"),
                HuffmanTreeNode::Branch(offset) => offset,
            };
            
            node_index += offset + ((code >> length) & 1);
        }

        match self.tree[node_index] {
            HuffmanTreeNode::Empty => self.tree[node_index] = HuffmanTreeNode::Leaf(symbol),
            HuffmanTreeNode::Leaf(_) => panic!("Can't assign"),
            HuffmanTreeNode::Branch(_offset) => panic!("{:?}, {:#b}: overriding branch??", self, code_length),
        }
    }

    pub(crate) fn build_implicit(code_lengths: Vec<u16>) -> HuffmanTree {
        let mut num_symbols = 0;
        let mut root_symbol = 0;

        for symbol in 0..code_lengths.len() {
            if code_lengths[symbol] > 0 {
                num_symbols += 1;
                root_symbol = symbol.try_into().unwrap();
            }
        }

        let mut tree = HuffmanTree::init(num_symbols);

        if num_symbols == 1 {
            tree.add_symbol(root_symbol, 0, 0);
        } else {

            let codes = HuffmanTree::code_lengths_to_codes(&code_lengths);

            for (symbol, &length) in code_lengths.iter().enumerate() {
                if length > 0 && codes[symbol].is_some() {
                    tree.add_symbol(symbol.try_into().unwrap(), codes[symbol].unwrap(), length);
                }
            }
        }

        tree
    }

    pub(crate) fn build_explicit(code_lengths: Vec<u16>, codes: Vec<u16>, symbols: Vec<u16>) -> HuffmanTree {
        let mut tree = HuffmanTree::init(symbols.len());

        for i in 0..symbols.len() {
            if codes[i] >= 0 {
                tree.add_symbol(symbols[i], codes[i], code_lengths[i]);
            }
        }

        tree
    }

    pub(crate) fn read_symbol(&self, bit_reader: &mut BitReader) -> u16 {
        let mut index = 0;
        let mut node = self.tree[index];

        while let HuffmanTreeNode::Branch(children_offset) = node {
            index += children_offset + bit_reader.read_bits::<usize>(1);
            node = self.tree[index];
        }

        match node {
            HuffmanTreeNode::Branch(_) => unreachable!(),
            HuffmanTreeNode::Empty => panic!("djiwod"),
            HuffmanTreeNode::Leaf(symbol) => symbol,
        }
    }
}






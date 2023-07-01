use std::convert::TryInto;

use super::lossless::BitReader;
use super::lossless::DecoderError;
use crate::ImageResult;

/// Rudimentary utility for reading Canonical Huffman Codes.
/// Based off https://github.com/webmproject/libwebp/blob/7f8472a610b61ec780ef0a8873cd954ac512a505/src/utils/huffman.c
///

const MAX_ALLOWED_CODE_LENGTH: usize = 15;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HuffmanTreeNode {
    Branch(usize), //offset in vector to children
    Leaf(u16),     //symbol stored in leaf
    Empty,
}

/// Huffman tree
#[derive(Clone, Debug, Default)]
pub(crate) struct HuffmanTree {
    tree: Vec<HuffmanTreeNode>,
    max_nodes: usize,
    num_nodes: usize,
}

impl HuffmanTree {
    fn is_full(&self) -> bool {
        self.num_nodes == self.max_nodes
    }

    /// Turns a node from empty into a branch and assigns its children
    fn assign_children(&mut self, node_index: usize) -> usize {
        let offset_index = self.num_nodes - node_index;
        self.tree[node_index] = HuffmanTreeNode::Branch(offset_index);
        self.num_nodes += 2;

        offset_index
    }

    /// Init a huffman tree
    fn init(num_leaves: usize) -> ImageResult<HuffmanTree> {
        if num_leaves == 0 {
            return Err(DecoderError::HuffmanError.into());
        }

        let max_nodes = 2 * num_leaves - 1;
        let tree = vec![HuffmanTreeNode::Empty; max_nodes];
        let num_nodes = 1;

        let tree = HuffmanTree {
            tree,
            max_nodes,
            num_nodes,
        };

        Ok(tree)
    }

    /// Converts code lengths to codes
    fn code_lengths_to_codes(code_lengths: &[u16]) -> ImageResult<Vec<Option<u16>>> {
        let max_code_length = *code_lengths
            .iter()
            .reduce(|a, b| if a >= b { a } else { b })
            .unwrap();

        if max_code_length > MAX_ALLOWED_CODE_LENGTH.try_into().unwrap() {
            return Err(DecoderError::HuffmanError.into());
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

        for (symbol, &length) in code_lengths.iter().enumerate() {
            let length = usize::from(length);
            if length > 0 {
                huff_codes[symbol] = next_codes[length];
                if let Some(value) = next_codes[length].as_mut() {
                    *value += 1;
                }
            } else {
                huff_codes[symbol] = None;
            }
        }

        Ok(huff_codes)
    }

    /// Adds a symbol to a huffman tree
    fn add_symbol(&mut self, symbol: u16, code: u16, code_length: u16) -> ImageResult<()> {
        let mut node_index = 0;
        let code = usize::from(code);

        for length in (0..code_length).rev() {
            if node_index >= self.max_nodes {
                return Err(DecoderError::HuffmanError.into());
            }

            let node = self.tree[node_index];

            let offset = match node {
                HuffmanTreeNode::Empty => {
                    if self.is_full() {
                        return Err(DecoderError::HuffmanError.into());
                    }
                    self.assign_children(node_index)
                }
                HuffmanTreeNode::Leaf(_) => return Err(DecoderError::HuffmanError.into()),
                HuffmanTreeNode::Branch(offset) => offset,
            };

            node_index += offset + ((code >> length) & 1);
        }

        match self.tree[node_index] {
            HuffmanTreeNode::Empty => self.tree[node_index] = HuffmanTreeNode::Leaf(symbol),
            HuffmanTreeNode::Leaf(_) => return Err(DecoderError::HuffmanError.into()),
            HuffmanTreeNode::Branch(_offset) => return Err(DecoderError::HuffmanError.into()),
        }

        Ok(())
    }

    /// Builds a tree implicitly, just from code lengths
    pub(crate) fn build_implicit(code_lengths: Vec<u16>) -> ImageResult<HuffmanTree> {
        let mut num_symbols = 0;
        let mut root_symbol = 0;

        for (symbol, length) in code_lengths.iter().enumerate() {
            if *length > 0 {
                num_symbols += 1;
                root_symbol = symbol.try_into().unwrap();
            }
        }

        let mut tree = HuffmanTree::init(num_symbols)?;

        if num_symbols == 1 {
            tree.add_symbol(root_symbol, 0, 0)?;
        } else {
            let codes = HuffmanTree::code_lengths_to_codes(&code_lengths)?;

            for (symbol, &length) in code_lengths.iter().enumerate() {
                if length > 0 && codes[symbol].is_some() {
                    tree.add_symbol(symbol.try_into().unwrap(), codes[symbol].unwrap(), length)?;
                }
            }
        }

        Ok(tree)
    }

    /// Builds a tree explicitly from lengths, codes and symbols
    pub(crate) fn build_explicit(
        code_lengths: Vec<u16>,
        codes: Vec<u16>,
        symbols: Vec<u16>,
    ) -> ImageResult<HuffmanTree> {
        let mut tree = HuffmanTree::init(symbols.len())?;

        for i in 0..symbols.len() {
            tree.add_symbol(symbols[i], codes[i], code_lengths[i])?;
        }

        Ok(tree)
    }

    /// Reads a symbol using the bitstream
    pub(crate) fn read_symbol(&self, bit_reader: &mut BitReader) -> ImageResult<u16> {
        let mut index = 0;
        let mut node = self.tree[index];

        while let HuffmanTreeNode::Branch(children_offset) = node {
            index += children_offset + bit_reader.read_bits::<usize>(1)?;
            node = self.tree[index];
        }

        let symbol = match node {
            HuffmanTreeNode::Branch(_) => unreachable!(),
            HuffmanTreeNode::Empty => return Err(DecoderError::HuffmanError.into()),
            HuffmanTreeNode::Leaf(symbol) => symbol,
        };

        Ok(symbol)
    }
}

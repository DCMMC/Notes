#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CS224N 2018-19: Homework 5
"""
import torch
import torch.nn as nn


### YOUR CODE HERE for part 1i

class CNN(nn.Module):
    def __init__(self, e_char, e_word, k):
        """
        @params e_char: char embedding size
        @params e_word: word embedding size
        @params k: kernel size
        """
        super(CNN, self).__init__()
        self.conv = nn.Conv1d(e_char, e_word, k)
        # 21 is the seq len (m_word)
        # Because we need do a globalmaxpooling
        self.max_pool = nn.MaxPool1d(21 - k + 1)

    def forward(self, x):
        """
        Map from x (aka., x_reshaped) to x_conv_out
        @params x: tensor with shape (N, C, L) where N the batch size, C the channels
                   (e_char in the case of HW5), and L the seq len (m_word in this case)
        @returns : tensor with shpae (N, C2) where C2 is the embedding size of the whole word
        """
        x_conv = self.conv(x)
        # i.e., a GlobalMaxPool1d
        x_conv_out = self.max_pool(x_conv).squeeze(-1)
        return x_conv_out

### END YOUR CODE


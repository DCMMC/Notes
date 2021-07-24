#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CS224N 2018-19: Homework 5
"""
import torch
import torch.nn as nn


### YOUR CODE HERE for part 1h
class Highway(nn.Module):
    """Implementation of Highway"""
    def __init__(self, e_word, dropout_rate, dtype=torch.float32):
        super(Highway, self).__init__()
        self.proj = nn.Linear(e_word, e_word, dtype=dtype)
        self.gate = nn.Linear(e_word, e_word, dtype=dtype)
        self.dropout = nn.Dropout(p=dropout_rate)
        self.relu = nn.ReLU()
        self.sigmoid = nn.Sigmoid()

    def forward(self, x):
        """
        @params x: input tensor with shape (*, H) where H is the hidden dimension of the
                   input feature (e_word in the case of HW5).
        """
        x_proj = self.relu(self.proj(x))
        x_gate = self.sigmoid(self.gate(x))
        x_highway = x_gate * x_proj + (1 - x_gate) * x
        x = self.dropout(x_highway)
        return x

### END YOUR CODE 


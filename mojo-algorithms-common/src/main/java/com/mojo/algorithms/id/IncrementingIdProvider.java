package com.mojo.algorithms.id;

public class IncrementingIdProvider implements IdProvider {
    int count = 1;
    @Override
    public String next() {
        return "T" + (count++);
    }
}

package net.ulfhedinn.util.counter

trait FrequencyCounter[A] {
    def increment(x: A) : Int
    def count(x: A) : Int
}
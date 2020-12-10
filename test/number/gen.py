import math
import sys


def gen_digits(digits, size):
    if size <= 0:
        raise Exception("size must be positive")
    elif size == 1:
        for digit in digits:
            yield digit
    else:
        for base in gen_digits(digits, size - 1):
            for digit in digits:
                yield base + digit


def gen(max_length, code, base, digits):
    for length in range(1, max_length + 1):
        min_bits = math.floor(1 + (length - 1) * math.log(base, 2))
        max_bits = math.ceil(length * math.log(base, 2))
        for number in gen_digits(digits, length):
            yield "'" + code + number
            yield "'s" + code + number

            number_bin = number.replace("x", "0").replace("z", "0")
            min_value = max(1, int(number_bin, base)) + 1
            curr_min_bits = max(min_bits, math.ceil(math.log(min_value, 2)))

            for bits in range(int(curr_min_bits), int(max_bits + 1)):
                size = str(bits)
                yield size + "'" + code + number
                yield size + "'s" + code + number


if __name__ == "__main__":
    assert len(sys.argv) == 5
    max_length = int(sys.argv[1])
    code = sys.argv[2]
    base = int(sys.argv[3])
    digits = sys.argv[4]

    print('`define T(n) $display(`"n => %b => %0d`", n, $bits(n));')
    print("module top;")
    print("initial begin")

    for number in gen(max_length, code, base, digits):
        print("`T({})".format(number))

    print("end")
    print("endmodule")

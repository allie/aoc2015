#include <iostream>
#include <string>

const std::string alphabet = "abcdefghijklmnopqrstuvwxyz";

int right_index(const std::string& input, int index) {
    return index + 1 > input.size() - 1 ?  0 : index + 1;
}

int left_index(const std::string& input, int index) {
    return index - 1 < 0 ? input.size() - 1 : index - 1;
}

void increment(std::string& input) {
    auto i = input.size() - 1;
    auto wrapped = false;

    do {
        auto f = alphabet.find(input[i]); // This should never be npos
        auto j = right_index(alphabet, f);
        input[i] = alphabet[j];

        if (j == 0) {
            wrapped = true;
            i = left_index(input, i);
        } else {
            wrapped = false;
        }
    } while (wrapped);
}

bool check(const std::string& input) {
    if (input.find("i") != std::string::npos ||
        input.find("o") != std::string::npos ||
        input.find("l") != std::string::npos) {
        return false;
    }

    int doubles = 0;

    for (int i = 0; i < input.size() - 1;) {
        if (input[i] == input[i + 1]) {
            doubles++;
            i += 2;
        } else {
            i++;
        }
    }

    if (doubles < 2) {
        return false;
    }

    for (int i = 0; i < alphabet.size() - 2; i++) {
        auto sub = alphabet.substr(i, 3);

        if (input.find(sub) != std::string::npos) {
            return true;
        }
    }

    return false;
}

int main() {
    std::string input = "cqjxjnds";

    while (!check(input)) {
        increment(input);
    }

    do {
        increment(input);
    } while (!check(input));

    std::cout << input << std::endl;
}

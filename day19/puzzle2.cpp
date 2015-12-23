#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <regex>
#include <fstream>
#include <limits>
#include <algorithm>

void replace_first(std::string& input,
                   const std::string& find,
                   const std::string& replace) {
    auto i = input.find(find);
    if (i != std::string::npos && i < input.size()) {
        input.replace(i, find.size(), replace);
    }
}

void recurse(const std::string& input,
             const std::vector<std::pair<std::string, std::string>>& pairs,
             int count,
             const std::string& target,
             int& min) {

    // If e has been reached, break out
    if (min != std::numeric_limits<int>::max()) {
        return;
    }

    // Reached target string
    if (input.compare(target) == 0) {
        if (count < min) {
            min = count;
            return;
        }
    }

    for (auto itr = pairs.begin(); itr != pairs.end(); itr++) {
        auto input_copy = std::string(input);
        auto find = itr->first;
        auto replace = itr->second;

        replace_first(input_copy, find, replace);

        if (input.compare(input_copy) != 0) {
            recurse(input_copy, pairs, count + 1, target, min);
        }
    }
}

auto parse(const std::string path, std::string& input) {
    auto ret = std::vector<std::pair<std::string, std::string>>();

    std::ifstream in;
    in.open(path);

    if (!in.good()) {
        return ret;
    }

    std::string line;
    auto r = std::regex(R"(^([A-Za-z]+)\s=>\s([A-Za-z]+))");
    auto i = std::regex(R"(^([A-Za-z]+)$)");
    std::smatch m;

    while (!in.eof()) {
        std::getline(in, line);

        // Replacements
        if (std::regex_search(line, m, r)) {
            ret.push_back(std::pair<std::string, std::string>(m[2], m[1]));
        }

        // Initial string
        else if (std::regex_search(line, m, i)) {
            input = m[1];
        }
    }

    in.close();
    return ret;
}

int main() {
    std::string input;
    auto pairs = parse("input.txt", input);
    auto min = std::numeric_limits<int>::max();
    recurse(input, pairs, 0, "e", min);
    std::cout << "Minimum: " << min << std::endl;
}

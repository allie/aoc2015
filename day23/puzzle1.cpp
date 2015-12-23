#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>

unsigned a = 0;
unsigned b = 0;

void exec(const std::vector<std::string>& instructions) {
    auto itr = instructions.begin();

    std::smatch m;
    auto hlf = std::regex("^hlf (a|b)");
    auto tpl = std::regex("^tpl (a|b)");
    auto inc = std::regex("^inc (a|b)");
    auto jmp = std::regex("^jmp (\\+|-)(\\d+)");
    auto jie = std::regex("^jie (a|b), (\\+|-)(\\d+)");
    auto jio = std::regex("^jio (a|b), (\\+|-)(\\d+)");

    while (itr < instructions.end()) {
        std::cout << *itr << "   a = " << a << " b = " << b << std::endl;
        // hlf r
        if (std::regex_search(*itr, m, hlf)) {
            unsigned& reg = m[1].compare("a") == 0 ? a : b;
            reg /= 2;
        }

        // tpl r
        else if (std::regex_search(*itr, m, tpl)) {
            unsigned& reg = m[1].compare("a") == 0 ? a : b;
            reg *= 3;
        }

        // inc r
        else if (std::regex_search(*itr, m, inc)) {
            unsigned& reg = m[1].compare("a") == 0 ? a : b;
            reg++;
        }

        // jmp offset
        else if (std::regex_search(*itr, m, jmp)) {
            auto n = m[1].compare("-") == 0 ? -1 : 1;
            itr += std::stoi(m[2]) * n;
            continue;
        }

        // jie r, offset
        else if (std::regex_search(*itr, m, jie)) {
            unsigned& reg = m[1].compare("a") == 0 ? a : b;
            auto n = m[2].compare("-") == 0 ? -1 : 1;
            if (reg % 2 == 0) {
                itr += std::stoi(m[3]) * n;
                continue;
            }
        }

        // jio r, offset
        else if (std::regex_search(*itr, m, jio)) {
            unsigned& reg = m[1].compare("a") == 0 ? a : b;
            auto n = m[2].compare("-") == 0 ? -1 : 1;
            if (reg == 1) {
                itr += std::stoi(m[3]) * n;
                continue;
            }
        }

        itr++;
    }
}

std::vector<std::string> parse(const std::string& path) {
    auto ret = std::vector<std::string>();

    std::ifstream in;
    in.open(path);
    if (!in.good()) {
        return ret;
    }

    std::string line;
    while (!in.eof()) {
        getline(in, line);
        if (line.size() >= 5) {
            ret.push_back(line);
        }
    }

    in.close();
	return ret;
}

int main() {
    auto instructions = parse("input.txt");
    exec(instructions);
    std::cout << b << std::endl;
}

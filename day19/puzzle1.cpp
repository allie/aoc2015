#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <regex>
#include <fstream>

int exec(const std::string& input, const std::vector<std::pair<std::string, std::string>>& replacements) {
	auto strings = std::map<std::string, int>();

	for (auto itr = replacements.begin(); itr != replacements.end(); itr++) {
		auto find = itr->first;
		auto replace = itr->second;
		auto i = input.find(find);

		while (i != std::string::npos && i < input.size()) {
			auto new_str = std::string(input);
			new_str.replace(i, find.size(), replace);
			strings.emplace(new_str, 0);
			i = input.find(find, i + find.size());
		}
	}

	return strings.size();
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
			ret.push_back(std::pair<std::string, std::string>(m[1], m[2]));
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
	auto replacements = parse("input.txt", input);
	std::cout << exec(input, replacements) << std::endl;
}

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <regex>

using namespace std;

// All permutations of c
auto perms(const vector<vector<int>>& c) {
	auto ret = vector<vector<int>>();

	for (auto itr = c.begin(); itr != c.end(); itr++) {
		auto sorted = vector<int>(*itr);
		sort(sorted.begin(), sorted.end());
		do {
			ret.push_back(vector<int>(sorted));
		} while (next_permutation(sorted.begin(), sorted.end()));
	}

	return ret;
}

// All combinations of length 4 in [0..n] with a sum of n
auto combs(int n) {
	auto ret = vector<vector<int>>();

	for (int a = 0; a <= n; a++) {
		for (int b = a; b <= n - a; b++) {
			for (int c = b; c <= n - (a + b); c++) {
				int d = n - (a + b + c);
				if (d >= c) {
					auto comb = vector<int> { d, c, b, a };
					ret.push_back(comb);
				}
			}
		}
	}

	return ret;
}

int calc(const vector<int>& amounts, const vector<vector<int>>& props) {
	int ret = 1;

	for (int j = 0; j < 4; j++) {
		int n = 0;

		for (int i = 0; i < 4; i++) {
			n += amounts[i] * props[j][i];
		}

		ret *= n < 0 ? 0 : n;
	}

	int c = 0;
	for (int i = 0; i < 4; i++) {
		c += amounts[i] * props[4][i];
	}

	return c == 500 ? ret : 0;
}

auto parse(const string path) {
	auto ret = vector<vector<int>>();
	for (int i = 0; i < 5; i++) {
		ret.push_back(vector<int>());
	}

	ifstream in;
	in.open(path);
	if (!in.good()) {
		return ret;
	}

	string line;
	// This is still disgusting
	auto r = regex(R"(^\w*:\s\w*\s(-?\d+),\s\w*\s(-?\d+),\s\w*\s(-?\d+),\s\w*\s(-?\d+),\s\w*\s(-?\d+))");
	smatch m;
	while (!in.eof()) {
		getline(in, line);
		cout << line << endl;
		if (regex_search(line, m, r)) {
			auto l = vector<int>();
			for (int i = 1; i < 6; i++) {
				ret[i - 1].push_back(stoi(m[i]));
				cout << m[i] << " ";
			}
			cout << endl;
		}
	}

	in.close();
	return ret;
}

int main() {
	auto l = parse("input.txt");
	auto c = combs(100);
	auto p = perms(c);

	int max = 0;
	for (auto itr = p.begin(); itr != p.end(); itr++) {
		int v = calc(*itr, l);

		if (v > max) {
			max = v;
		}
	}

	cout << max << endl;
}

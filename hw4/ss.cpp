#include <iostream>
#include <vector>
#include <algorithm>

using uint_vector = std::vector<unsigned>;

uint_vector sieve_sundaram(unsigned n)
{
    uint_vector result;
    result.reserve(2 * n);
    std::vector<bool> filter(2 * n + 3, false);
    for(unsigned i = 1; i <= n; ++i)
    {
        unsigned m = (n - i) / (2 * i + 1);
        for(unsigned j = i; j <= m; ++j)
        {
            filter[i + j + 2 * i * j] = true;
        }
    }

    for(unsigned i = 1; i <= (2 * n + 2); ++i)
    {
        if(filter[i] == false)
        {
            result.push_back(2*i + 1);
        }
    }

    return result;
}

int main()
{
    auto n = 100000000u;
    auto primes = sieve_sundaram(n);
    std::cout << "Sundaram gave " << primes.size() << " for " << n << "\n";
    return 0;
}
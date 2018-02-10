#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <boost/function_output_iterator.hpp>
#include <boost/range/irange.hpp>

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

    std::function<void(unsigned)> to_prime = [&result](unsigned i) {
        result.emplace_back(2*i+1);
    };
    auto output_it = boost::make_function_output_iterator(to_prime);
    auto irange = boost::irange(1u, n);
    std::copy_if(irange.begin(), irange.end(), output_it,
        [&filter](unsigned i)
        {
            return filter[i] == false;
        }
    );

    return result;
}

int main()
{
    auto n = 100000000u;
    auto primes = sieve_sundaram(n);
    std::cout << "Sundaram gave " << primes.size() << " for " << n << "\n";
    return 0;
}
function fibonacci(n)
    if n<=3 then
        return 1
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

fibonacci(28)
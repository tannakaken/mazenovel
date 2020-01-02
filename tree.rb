require "json"

tree = {node: nil}
novels = Dir.glob("novels/*")
novels.each do |file|
  File.open(file) do |f|
    tail = tree
    line = f.readline.strip
    line.each_char do |c|
      if tail.has_key? :next  then
        passed = false
        tail[:next].each do |branch|
          if branch[:node] == c then
            tail = branch
            passed = true
            break
          end
        end
        if passed
          next
        end
        next_tail = {node: c}
        tail[:next].push next_tail
        tail = next_tail
      else
        next_tail = {node: c}
        tail[:next] = [next_tail]
        tail = next_tail
      end
    end
  end
end

File.open("public/tree.json", "w") do |file|
  JSON.dump(tree, file)
end

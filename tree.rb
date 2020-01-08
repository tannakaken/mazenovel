require "json"

tree = [{c: nil, n: []}]
novels = Dir.glob("novels/*")
novels.each do |file|
  File.open(file) do |f|
    currentIndex = 0
    line = f.readline.strip
    line.each_char do |c|
      currentNode = tree[currentIndex]
      passed = false
      currentNode[:n].each do |branchIndex|
        branchNode = tree[branchIndex]
        if branchNode[:c] == c then
          currentIndex = branchIndex
          passed = true
          break
        end
      end
      if passed
        next
      end
      newBranchNode = {c: c, n: []}
      newBranchIndex = tree.size
      tree.push newBranchNode
      currentNode[:n].push newBranchIndex
      currentIndex = newBranchIndex
    end
  end
end

File.open("public/tree.json", "w") do |file|
  JSON.dump(tree, file)
end

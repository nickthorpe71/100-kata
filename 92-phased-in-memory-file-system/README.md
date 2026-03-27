# 92 Phased In-Memory File System

## Format
Evolving requirements with object-oriented design.

## Time Target
45-60 minutes.

## Phase 1
Implement:
- `mkdir(path)`
- `addContentToFile(filePath, content)`
- `readContentFromFile(filePath)`

## Phase 2
Add `ls(path)`:
- if `path` is a directory, return names in lexicographic order
- if `path` is a file, return just the file name

## Phase 3
Add `move(sourcePath, destinationPath)` and explain which invariants and edge cases matter most.

## Starting Skeleton

```cpp
class FileSystem {
public:
    std::vector<std::string> ls(const std::string& path);
    void mkdir(const std::string& path);
    void addContentToFile(const std::string& filePath, const std::string& content);
    std::string readContentFromFile(const std::string& filePath);
    void move(const std::string& sourcePath, const std::string& destinationPath);
};
```

## Interview Focus
- tree-shaped data modeling
- path parsing
- extending behavior without breaking existing operations

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSLtem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FsZipper -> FSZipper
fsUp (item, FSCrumb name li rs:bs) = (Folder name (ls ++ [Item] ++ rs), bs)

import Data.List (break)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name item, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

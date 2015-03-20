package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
)

type byBase []string

// Implement sort.Interface {{{1
func (b byBase) Len() int {
	return len(b)
}

func (b byBase) Less(i, j int) bool {
	return filepath.Base(b[i]) < filepath.Base(b[j])
}

func (b byBase) Swap(i, j int) {
	b[i], b[j] = b[j], b[i]
}

// scanDir() {{{1
func scanDir(fnames *[]string, arg string) {
	dir, err := os.Open(arg)
	if err != nil {
		log.Fatal(err)
	}
	defer dir.Close()

	fis, err := dir.Readdir(-1)
	if err != nil {
		log.Fatal(err)
	}

	r, err := regexp.Compile("(?i)" + os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	for _, fi := range fis {
		fname := fi.Name()

		if !fi.IsDir() && r.MatchString(fname) {
			*fnames = append(*fnames, arg+"/"+fname)
		} else if fi.IsDir() {
			scanDir(fnames, arg+"/"+fname)
		}
	}
}

// run() {{{1
func run(prog, bookpath string) {
	cmd := exec.Command(prog, bookpath)
	err := cmd.Start()
	if err != nil {
		log.Fatal(err)
	}
}

// main() {{{1
func main() {
	if len(os.Args) < 2 {
		fmt.Printf("usage: %s <regex>\n", os.Args[0])
		os.Exit(0)
	}

	var fnames []string
	scanDir(&fnames, "/data/books")

	if len(fnames) == 0 {
		fmt.Println("\033[31mNothing found!")
		return
	}

	sort.Sort(byBase(fnames))
	for i, v := range fnames {
		fmt.Printf(" \033[32m%-5d \033[33m%s\n", i, filepath.Base(v))
	}

	var input int
	fmt.Print("\033[32mSelect>\033[0m ")
	_, err := fmt.Scanf("%d", &input)
	if err != nil {
		log.Fatal(err)
	}

	switch filepath.Ext(fnames[input]) {
	case ".pdf":
		run("zathura", fnames[input])
	case ".chm":
		run("xchm", fnames[input])
	case ".djvu":
		run("djview", fnames[input])
	default:
		fmt.Println("\033[31mI can't handle that file!")
	}
}

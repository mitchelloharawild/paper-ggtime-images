# Images for Crossmaps Project


## Submoduling Instructions

Run these commands from inside the target repo (i.e. the one you want
these images to be available in)

To add the contents of the latest commit of the default branch of this
repo as the folder `images/`:

``` zsh
git submodule add <repo-url.git> images
```

Updating contents to match the latest commit:

``` zsh
git submodule update --remote --merge
```

to match remote:

``` zsh
git submodule update --remote --rebase
```

## Folder & File Naming conventions

Folders:

- `illustrations` is for anything hand-drawn
- `screenshots` is for screenshots
- `plots` is for programmatically generated plots (e.g. using ggplot2)
- `graphics` is for diagrams etc produced using figma, pptx etc.

File prefixes:

- `diagram_`
- `icon_`

## Useful ImageMagick Commands

Crop & transparent background:

``` zsh
filename=filename.png
magick $filename -trim -transparent white $filename
```

For details see:
<https://www.cynthiahqy.com/posts/imagemagick-basic-trim/>

## Setup pre-commit hook

To make sure this README renders every time you add a new commit, move
the file `pre-commit` to `.git/hooks/` and make the script executable:
`chmod +x .git/hooks/pre-commit`

To skip the render when you commit add the `--no-verify` flag:

    git commit --no-verify

## Images

## Graphics

## Illustrations

## Plots

## Screenshots

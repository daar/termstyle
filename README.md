# TermStyle

TermStyle is a Pascal library to add **colorful and styled terminal output** to CLI applications. It supports **text attributes**, **foreground/background colors**, and **nested tags**, similar to CSS but for the terminal. You can also print banners, info/warning/error messages, and highlight important text easily.

![example](./img/screenshot.png)


## Usage

Wrap text in `<tag>` and `</tag>`:

```pascal
uses
  TermStyle;

Writeln(parse('<div class="font-bold text-red">Important!</div>'));
Writeln(parse('<div class="text-green-500 bg-gray-800">Green text on dark background</div>'));
````

You can combine multiple styles:

```pascal
Writeln(parse('<div class="font-bold underline text-blue-500">Bold, Underlined, Blue</div>'));
```


### Basic Demo

```pascal
program TermStyleBasicDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, 
  TermStyle;

begin
  tsInfo('Welcome to the TermStyle demo! This showcases colorful terminal styling.');
  tsSuccess('Installation completed successfully!');
  tsWarn('Low disk space detected. Consider cleaning up.');
  tsError('Failed to connect to the server. Please check your network.');

  Writeln(parse('<div class="font-bold font-underline">Bold & Underlined</div>'));
  Writeln(parse('<div><div class="text-red-500">Red text </div><div class="text-green-500">Green text </div><div class="text-blue-500">Blue text </div></div>'));
  Writeln(parse('<div><div class="bg-yellow-300 text-black">Black on bright yellow background</div></div>'));
  Writeln(parse('<div><div class="text-pink-400 italic">Italic bright magenta text</div></div>'));
  Writeln(parse('<div class="invert">Reversed colors text</div>'));
end.
```

## Installation

### With Nova package manager

If you are using [Nova](https://github.com/nova-packager/nova), you can install directly:

```sh
nova install daar/termstyle
```

Then include it in your project:

```pascal
uses
  termstyle;
```

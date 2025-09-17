# TermStyle

TermStyle is a Pascal library to add **colorful and styled terminal output** to CLI applications. It supports **text attributes**, **foreground/background colors**, and **nested tags**, similar to CSS but for the terminal. You can also print banners, info/warning/error messages, and highlight important text easily.

![example](./img/screenshot.png)


## Usage

Wrap text in `<tag>` and `</tag>`:

```pascal
Writeln(FormatText('<bold><red>Important!</red></bold>'));
Writeln(FormatText('<green bg:bright_black>Green text on dark background</green>'));
````

You can combine multiple styles:

```pascal
Writeln(FormatText('<bold><underline><blue>Bold, Underlined, Blue</blue></underline></bold>'));
```


### Basic Demo

```pascal
program TermStyleBasicDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, termstyle;

begin
  PrintInfo('Welcome to the TermStyle demo! This showcases colorful terminal styling.');
  PrintSuccess('Installation completed successfully!');
  PrintWarn('Low disk space detected. Consider cleaning up.');
  PrintError('Failed to connect to the server. Please check your network.');

  Writeln(FormatText('<bold><underline>Bold & Underlined</underline></bold>'));
  Writeln(FormatText('<red>Red text</red> <green>Green text</green> <blue>Blue text</blue>'));
  Writeln(FormatText('<bg:bright_yellow black>Black on bright yellow background</bg:bright_yellow>'));
  Writeln(FormatText('<italic><bright_magenta>Italic bright magenta text</bright_magenta></italic>'));
  Writeln(FormatText('<reversed>Reversed colors text</reversed>'));
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

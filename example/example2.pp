program example2;

{$mode objfpc}{$H+}

uses
  SysUtils, termstyle;

begin
  // --- Banner ---
  PrintBanner('<white bg:bright_blue bold>', '</white>', 'TERMSTYLE DEMO CLI');
  writeln;

  // --- Info block ---
  PrintInfo('Welcome to the TermStyle demo! This showcases colorful terminal styling.');

  // --- Success message ---
  PrintSuccess('Installation completed successfully!');

  // --- Warning message ---
  PrintWarn('Low disk space detected. Consider cleaning up.');

  // --- Error message ---
  PrintError('Failed to connect to the server. Please check your network.');
  writeln;

  // --- Styled text examples ---
  Writeln(FormatText('<bold><underline>Bold & Underlined</underline></bold>'));
  Writeln(FormatText('<red>Red text</red> <green>Green text</green> <blue>Blue text</blue>'));
  Writeln(FormatText('<bg:bright_yellow black>Black on bright yellow background</bg:bright_yellow>'));
  Writeln(FormatText('<italic><bright_magenta>Italic bright magenta text</bright_magenta></italic>'));
  Writeln(FormatText('<reversed>Reversed colors text</reversed>'));

  // --- Simulated menu ---
  Writeln('');
  Writeln(FormatText('<bright_cyan>Available Commands:</bright_cyan>'));
  Writeln(FormatText('  <green>install</green> Install the package'));
  Writeln(FormatText('  <yellow>update</yellow>  Update to latest version'));
  Writeln(FormatText('  <red>remove</red>  Remove the package'));
  Writeln(FormatText('  <magenta>help</magenta>    Show help'));

  // --- Prompt example ---
  Writeln('');
  Write(FormatText('<bright_magenta>Enter command></bright_magenta> '));
  WriteLn('<user input simulation>');  // just for screenshot
  writeln;

  // --- Footer ---
  PrintBanner('<white bg:bright_blue bold>', '</white>', 'END OF DEMO');
end.


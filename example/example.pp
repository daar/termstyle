program example;

{$mode objfpc}{$H+}

uses
  termstyle, SysUtils;

var
  name, choice: string;

procedure ShowMenu;
begin
  Writeln(FormatText('  <bright_cyan>[1]</bright_cyan> <green>Run task</green>'));
  Writeln(FormatText('  <bright_cyan>[2]</bright_cyan> <yellow>Show warning</yellow>'));
  Writeln(FormatText('  <bright_cyan>[3]</bright_cyan> <red>Trigger error</red>'));
  Writeln(FormatText('  <bright_cyan>[4]</bright_cyan> <magenta>Exit</magenta>'));
end;

begin
  // Big colorful banner
  Writeln(FormatText('<bright_blue bold>==================================================</bright_blue>'));
  Writeln(FormatText('<bright_blue bold>        🌟 Welcome to TermStyle CLI Demo 🌟        </bright_blue>'));
  Writeln(FormatText('<bright_blue bold>==================================================</bright_blue>'));
  Writeln;

  // Intro
  PrintInfo('This demo showcases colorful terminal styling, like Laravel Artisan.');
  Writeln;

  // User prompt
  name := Prompt('What is your name');
  PrintSuccess('Hello, <bold><underline>' + name + '</underline></bold>! Ready to begin?');
  Writeln;

  // Menu loop
  repeat
    PrintBanner('<bright_white bg:bright_blue bold>', '</bright_white>', 'Main Menu');
    ShowMenu;
    Writeln;

    choice := Prompt('Select option');

    if choice = '1' then
    begin
      PrintInfo('Running background task...');
      Sleep(600); // simulate work
      PrintSuccess('✨ Task finished successfully!');
    end
    else if choice = '2' then
    begin
      PrintWarn('⚠️  Disk space is almost full. Please clean up soon!');
    end
    else if choice = '3' then
    begin
      PrintError('❌ Could not connect to database!');
    end
    else if choice = '4' then
    begin
      PrintInfo('👋 Exiting the demo...');
      Break;
    end
    else
    begin
      PrintError('Invalid choice: ' + choice);
    end;

    Writeln;
  until choice = '4';

  // Exit banner
  PrintBanner('<bright_white bg:bright_green>', '</bright_white>', 'Thank you for using TermStyle!');
end.


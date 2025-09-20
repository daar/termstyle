program example;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TermStyle;

var
  name, choice: string;

procedure ShowMenu;
begin
  Writeln(parse('  <div class="text-cyan-400">[1]</div> <div class="text-green-500">Run task</div>'));
  Writeln(parse('  <div class="text-cyan-400">[2]</div> <div class="text-yellow-500">Show warning</div>'));
  Writeln(parse('  <div class="text-cyan-400">[3]</div> <div class="text-red-500">Trigger error</div>'));
  Writeln(parse('  <div class="text-cyan-400">[4]</div> <div class="text-fuchsia-500">Exit</div>'));
end;

begin
  // Big colorful banner
  Writeln(parse('<div class="text-blue-400 font-bold">==================================================</div>'));
  Writeln(parse('<div class="text-blue-400 font-bold">        🌟 Welcome to TermStyle CLI Demo 🌟        </div>'));
  Writeln(parse('<div class="text-blue-400 font-bold">==================================================</div>'));
  Writeln;

  // Intro
  tsInfo('This demo showcases colorful terminal styling, like Laravel Artisan.');
  Writeln;

  // User prompt
  name := Prompt('What is your name');
  tsSuccess('Hello, <div class="font-bold underline">' + name + '</div>! Ready to begin?');
  Writeln;

  // Menu loop
  repeat
    tsBanner('<div class="text-white bg-blue-400 font-bold>', '</div>', 'Main Menu');
    ShowMenu;
    Writeln;

    choice := Prompt('Select option');

    if choice = '1' then
    begin
      tsInfo('Running background task...');
      Sleep(600); // simulate work
      tsSuccess('✨ Task finished successfully!');
    end
    else if choice = '2' then
    begin
      tsWarn('⚠️  Disk space is almost full. Please clean up soon!');
    end
    else if choice = '3' then
    begin
      tsError('❌ Could not connect to database!');
    end
    else if choice = '4' then
    begin
      tsInfo('👋 Exiting the demo...');
      Break;
    end
    else
    begin
      tsError('Invalid choice: ' + choice);
    end;

    Writeln;
  until choice = '4';

  // Exit banner
  tsBanner('<div class="text-white bg-green-400">', '</div>', 'Thank you for using TermStyle!');
end.


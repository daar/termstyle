program example;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TermStyle;

var
  name, choice: string;

procedure ShowMenu;
begin
  writeln(render('  <div class="text-cyan-400">[1]</div> <div class="text-green-500">Run task</div>'));
  writeln(render('  <div class="text-cyan-400">[2]</div> <div class="text-yellow-500">Show warning</div>'));
  writeln(render('  <div class="text-cyan-400">[3]</div> <div class="text-red-500">Trigger error</div>'));
  writeln(render('  <div class="text-cyan-400">[4]</div> <div class="text-fuchsia-500">Exit</div>'));
end;

begin
  // Big colorful banner
  writeln(render('<div class="text-blue-400 font-bold">==================================================</div>'));
  writeln(render('<div class="text-blue-400 font-bold">        🌟 Welcome to TermStyle CLI Demo 🌟        </div>'));
  writeln(render('<div class="text-blue-400 font-bold">==================================================</div>'));
  writeln;

  // Intro
  info('This demo showcases colorful terminal styling, like Laravel Artisan.');
  writeln;

  // User prompt
  name := Prompt('What is your name');
  success('Hello, <div class="font-bold underline">' + name + '</div>! Ready to begin?');
  writeln;

  // Menu loop
  repeat
    banner('<div class="text-white bg-blue-400 font-bold>', '</div>', 'Main Menu');
    ShowMenu;
    writeln;

    choice := Prompt('Select option');

    if choice = '1' then
    begin
      info('Running background task...');
      Sleep(600); // simulate work
      success('✨ Task finished successfully!');
    end
    else if choice = '2' then
    begin
      warning('⚠️  Disk space is almost full. Please clean up soon!');
    end
    else if choice = '3' then
    begin
      error('❌ Could not connect to database!');
    end
    else if choice = '4' then
    begin
      info('👋 Exiting the demo...');
      Break;
    end
    else
    begin
      error('Invalid choice: ' + choice);
    end;

    writeln;
  until choice = '4';

  // Exit banner
  banner('<div class="text-white bg-green-400">', '</div>', 'Thank you for using TermStyle!');
end.


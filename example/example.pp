program example;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TermStyle,
  TermStyle.Prompts;

var
  UserName: string;
  Choice: integer;
  Confirmed: boolean;
  ProgressBar: TProgressBar;
  i: integer;

begin
  // Intro
  intro('TermStyle CLI Demo');
  writeln;

  // User prompt with text input
  UserName := text(
    'What is your name?',
    'Enter your name',
    '',
    'This will be used to greet you.'
  );

  writeln(render('<span class="text-green-500">Hello, </span><span class="text-cyan-500 font-bold">' + UserName + '</span><span class="text-green-500">!</span>'));
  writeln;

  // Menu loop using select
  repeat
    Choice := select(
      'What would you like to do?',
      ['Run task', 'Show warning', 'Trigger error', 'Exit'],
      0,
      'Use arrow keys or TAB to navigate'
    );

    case Choice of
      0: // Run task
      begin
        if confirm('Run the background task?', true) then
        begin
          writeln;
          spin('Running background task...');
          Sleep(1000);
          spinStop(true, 'Task completed successfully!');

          // Show progress bar demo
          writeln;
          ProgressBar := progress('Processing items', 50);
          for i := 1 to 50 do
          begin
            Sleep(30);
            ProgressBar.Advance(1);
          end;
          ProgressBar.Finish;
          ProgressBar.Free;
        end;
      end;

      1: // Show warning
      begin
        alert('Disk space is almost full. Please clean up soon!');
      end;

      2: // Trigger error
      begin
        writeln;
        spin('Connecting to database...');
        Sleep(800);
        spinStop(false, 'Could not connect to database!');
        writeln;
      end;

      3: // Exit
      begin
        Confirmed := confirm('Are you sure you want to exit?', false);
        if not Confirmed then
          Choice := -1; // Don't exit, continue loop
      end;
    end;

    writeln;
  until Choice = 3;

  // Outro
  outro('Thank you for using TermStyle, ' + UserName + '!');
end.

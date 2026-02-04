program prompts_demo;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  TermStyle,
  TermStyle.Prompts;

var
  Name, Email, Country, Pwd: string;
  Framework: integer;
  Features: TStringList;
  Confirmed: boolean;
  ProgressBar: TProgressBar;
  i: integer;
  TableData: array of TStringArray;

const
  Countries: array[0..9] of string = (
    'Australia', 'Austria', 'Belgium', 'Brazil', 'Canada',
    'Denmark', 'Finland', 'France', 'Germany', 'Netherlands'
  );

begin
  // Intro
  intro('TermStyle Prompts Demo');
  writeln;

  // Text input with placeholder and default
  Name := TermStyle.Prompts.text(
    'What is your name?',
    'E.g. John Doe',   // placeholder
    '',                // default
    'This will be displayed in your profile.'  // hint
  );

  writeln(render('<span class="text-green-500">Hello, </span><span class="text-cyan-500 font-bold">' + Name + '</span><span class="text-green-500">!</span>'));
  writeln;

  // Text input with required validation
  Email := TermStyle.Prompts.text(
    'What is your email address?',
    'user@example.com',
    '',
    '',
    true  // required
  );
  writeln;

  // Suggest (autocomplete)
  Country := suggest(
    'What country are you from?',
    Countries,
    'Start typing...',
    '',
    'Type to search, TAB to autocomplete'
  );
  writeln(render('<span class="text-gray-500">Country: </span><span class="text-cyan-500">' + Country + '</span>'));
  writeln;

  // Password input (hidden)
  Pwd := TermStyle.Prompts.password(
    'Create a password',
    'Min 8 characters',
    'Your password will be encrypted.'
  );
  writeln(render('<span class="text-gray-500">Password length: </span><span class="text-cyan-500">' + IntToStr(Length(Pwd)) + ' chars</span>'));
  writeln;

  // Confirm
  Confirmed := confirm(
    'Do you accept the terms and conditions?',
    false,  // default
    'Accepted',
    'Declined',
    'Please read the terms before accepting.'
  );
  writeln;

  // Select
  Framework := TermStyle.Prompts.select(
    'Which framework do you prefer?',
    ['Free Pascal', 'Delphi', 'Lazarus', 'PascalABC', 'Oxygene'],
    0,  // default to first option
    'Select your favorite Pascal compiler.'
  );
  writeln(render('<span class="text-gray-500">Framework index: </span><span class="text-cyan-500">' + IntToStr(Framework) + '</span>'));
  writeln;

  // Multi-select
  Features := multiselect(
    'Which features would you like to enable?',
    ['Authentication', 'API Support', 'Queue Workers', 'Scheduler', 'Notifications'],
    'You can select multiple options.',
    true  // at least one required
  );

  writeln(render('<span class="text-gray-500">Selected features:</span>'));
  for i := 0 to Features.Count - 1 do
    writeln(render('  <span class="text-green-500">' + #$E2#$9C#$93 + '</span> ' + Features[i]));
  Features.Free;
  writeln;

  // Alert
  alert('Important: Remember to configure your .env file!');

  // Note
  note('Your application is almost ready.');
  writeln;

  // Table
  writeln(render('<span class="text-white font-bold">Configuration Summary:</span>'));
  writeln;

  SetLength(TableData, 3);
  SetLength(TableData[0], 2);
  TableData[0][0] := 'Name';
  TableData[0][1] := Name;
  SetLength(TableData[1], 2);
  TableData[1][0] := 'Email';
  TableData[1][1] := Email;
  SetLength(TableData[2], 2);
  TableData[2][0] := 'Terms';
  if Confirmed then
    TableData[2][1] := 'Accepted'
  else
    TableData[2][1] := 'Declined';

  TermStyle.Prompts.table(['Setting', 'Value'], TableData);
  writeln;

  // Progress bar
  ProgressBar := progress('Installing dependencies', 100);
  for i := 1 to 100 do
  begin
    Sleep(20);
    ProgressBar.Advance(1);
  end;
  ProgressBar.Finish;
  ProgressBar.Free;
  writeln;

  // Spinner demo
  spin('Configuring environment...');
  Sleep(1500);
  spinStop(true, 'Environment configured successfully!');
  writeln;

  // Pause before exit
  TermStyle.Prompts.pause('Press ENTER to exit...');

  // Outro
  outro('Thank you for trying TermStyle Prompts!');
end.

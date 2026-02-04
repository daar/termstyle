program example4;

{$mode objfpc}{$H+}

uses
  SysUtils,
  TermStyle;

const
  // Sample unified diff
  SampleDiff =
    'diff --git a/example.js b/example.js' + LineEnding +
    '--- a/example.js' + LineEnding +
    '+++ b/example.js' + LineEnding +
    '@@ -1,6 +1,6 @@' + LineEnding +
    '-const message = "Hello World";' + LineEnding +
    '+const message = "Hello, World!";' + LineEnding +
    ' ' + LineEnding +
    ' function greet(name) {' + LineEnding +
    '-  return message + " " + name;' + LineEnding +
    '+  return message + " " + name + "!";' + LineEnding +
    ' }' + LineEnding +
    ' ' + LineEnding +
    ' module.exports = { greet };';

begin
  writeln;
  banner('TermStyle Diff Demo', 'text-white bg-indigo-600 font-bold');
  writeln;

  // Demo 1: Using diff_file to parse unified diff
  info('Parsing a unified diff automatically:');
  writeln;
  diff_file(SampleDiff);
  writeln;

  // Demo 2: Manual diff line construction
  info('Building diff output manually:');
  writeln;

  diff_header('diff --git a/config.pas b/config.pas');
  diff_hunk('@@ -10,4 +10,5 @@');
  diff_context(10, 'type');
  diff_context(11, '  TConfig = record');
  diff_del(12, '    Debug: Boolean;');
  diff_add(12, '    Debug: Boolean;');
  diff_add(13, '    Verbose: Boolean;');
  diff_context(14, '  end;');
  writeln;

  // Demo 3: Show individual components
  info('Individual diff line styles:');
  writeln;

  writeln('  Added line:');
  diff_add(42, 'NewFeature := True;');
  writeln;

  writeln('  Deleted line:');
  diff_del(42, 'OldCode := False;');
  writeln;

  writeln('  Context line:');
  diff_context(43, 'CommonCode := True;');
  writeln;

  success('Diff rendering complete!');
end.

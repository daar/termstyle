unit TermStyle.Prompts;

{$mode ObjFPC}{$H+}

{ Interactive CLI prompts for Free Pascal

  This unit provides beautiful, interactive command-line prompts. It includes:

  - text()        : Text input with placeholder, default, hint
  - password()    : Masked password input
  - confirm()     : Yes/No confirmation
  - select()      : Single selection from options
  - multiselect() : Multiple selection from options
  - pause()       : Press ENTER to continue
  - note()        : Informational note
  - intro()       : Section intro header
  - outro()       : Section outro footer
  - alert()       : Alert box
  - spin()        : Loading spinner
  - progress()    : Progress bar
  - table()       : Formatted table display
}

interface

uses
  Classes, SysUtils;

type
  { Validation callback - returns empty string if valid, error message otherwise }
  TValidateFunc = function(const AValue: string): string;

  { TPromptOption - for select/multiselect }
  TPromptOption = record
    Value: string;
    Label_: string;
  end;
  TPromptOptions = array of TPromptOption;

{ Text input with Interactive formatting }
function text(
  const ALabel: string;
  const APlaceholder: string = '';
  const ADefault: string = '';
  const AHint: string = '';
  const ARequired: boolean = false
): string;

{ Text input with autocomplete suggestions }
function suggest(
  const ALabel: string;
  const ASuggestions: array of string;
  const APlaceholder: string = '';
  const ADefault: string = '';
  const AHint: string = ''
): string;

{ Password input (hidden) }
function password(
  const ALabel: string;
  const APlaceholder: string = '';
  const AHint: string = ''
): string;

{ Confirm (Yes/No) }
function confirm(
  const ALabel: string;
  const ADefault: boolean = false;
  const AYes: string = 'Yes';
  const ANo: string = 'No';
  const AHint: string = ''
): boolean;

{ Select single option - returns index }
function select(
  const ALabel: string;
  const AOptions: array of string;
  const ADefault: integer = 0;
  const AHint: string = ''
): integer;

{ Select single option with custom values - returns selected value }
function selectValue(
  const ALabel: string;
  const AOptions: TPromptOptions;
  const ADefault: integer = 0;
  const AHint: string = ''
): string;

{ Multi-select - returns array of selected indices }
function multiselect(
  const ALabel: string;
  const AOptions: array of string;
  const AHint: string = '';
  const ARequired: boolean = false
): TStringList;

{ Pause - wait for ENTER }
procedure pause(const AMessage: string = 'Press ENTER to continue...');

{ Informational outputs (Interactive) }
procedure note(const AMessage: string);
procedure intro(const ATitle: string);
procedure outro(const AMessage: string);
procedure alert(const AMessage: string);

{ Spinner for long operations }
procedure spin(const AMessage: string);
procedure spinStop(const ASuccess: boolean = true; const AMessage: string = '');

{ Progress bar }
type
  TProgressBar = class
  private
    FLabel: string;
    FTotal: integer;
    FCurrent: integer;
    FWidth: integer;
  public
    constructor Create(const ALabel: string; const ATotal: integer; const AWidth: integer = 40);
    procedure Advance(const AStep: integer = 1);
    procedure Finish;
    property Current: integer read FCurrent;
    property Total: integer read FTotal;
  end;

function progress(const ALabel: string; const ATotal: integer; const AWidth: integer = 40): TProgressBar;

{ Table display }
procedure table(const AHeaders: array of string; const ARows: array of TStringArray);

implementation

uses
  TermStyle.Color,
  {$IFDEF UNIX}
  BaseUnix,
  termio,
  {$ENDIF}
  Keyboard;

const
  { Box drawing characters }
  BOX_TOP_LEFT     = #$E2#$94#$8C;     // ┌
  BOX_TOP_RIGHT    = #$E2#$94#$90;     // ┐
  BOX_BOTTOM_LEFT  = #$E2#$94#$94;     // └
  BOX_BOTTOM_RIGHT = #$E2#$94#$98;     // ┘
  BOX_HORIZONTAL   = #$E2#$94#$80;     // ─
  BOX_VERTICAL     = #$E2#$94#$82;     // │
  BOX_CROSS        = #$E2#$94#$BC;     // ┼
  BOX_T_DOWN       = #$E2#$94#$AC;     // ┬
  BOX_T_UP         = #$E2#$94#$B4;     // ┴
  BOX_T_RIGHT      = #$E2#$94#$9C;     // ├
  BOX_T_LEFT       = #$E2#$94#$A4;     // ┤

  { Prompt indicators }
  CHEVRON          = #$E2#$80#$BA;     // ›
  BULLET           = #$E2#$97#$8F;     // ●
  BULLET_EMPTY     = #$E2#$97#$8B;     // ○
  CHECK            = #$E2#$9C#$93;     // ✓
  CROSS_MARK       = #$E2#$9C#$97;     // ✗

  { Spinner frames }
  SPINNER_FRAMES: array[0..9] of string = (
    #$E2#$A0#$8B,  // ⠋
    #$E2#$A0#$99,  // ⠙
    #$E2#$A0#$B9,  // ⠹
    #$E2#$A0#$B8,  // ⠸
    #$E2#$A0#$BC,  // ⠼
    #$E2#$A0#$B4,  // ⠴
    #$E2#$A0#$A6,  // ⠦
    #$E2#$A0#$A7,  // ⠧
    #$E2#$A0#$87,  // ⠇
    #$E2#$A0#$8F   // ⠏
  );

var
  SpinnerFrame: integer = 0;
  {$IFNDEF UNIX}
  KeyboardInitialized: boolean = false;
  {$ENDIF}

{$IFDEF UNIX}
var
  OldTermios: termio.Termios;
  RawModeActive: boolean = false;

procedure EnableRawMode;
var
  NewTermios: termio.Termios;
begin
  if RawModeActive then Exit;
  TCGetAttr(0, OldTermios);
  NewTermios := OldTermios;
  NewTermios.c_lflag := NewTermios.c_lflag and (not (ICANON or ECHO));
  NewTermios.c_cc[VMIN] := 1;
  NewTermios.c_cc[VTIME] := 0;
  TCSetAttr(0, TCSANOW, NewTermios);
  RawModeActive := true;
end;

procedure DisableRawMode;
begin
  if not RawModeActive then Exit;
  TCSetAttr(0, TCSANOW, OldTermios);
  RawModeActive := false;
end;

function ReadKeyRaw: char;
var
  c: char;
begin
  fpRead(0, c, 1);
  Result := c;
end;
{$ENDIF}

type
  TKeyCode = (
    kcNone,
    kcChar,
    kcEnter,
    kcEscape,
    kcBackspace,
    kcDelete,
    kcTab,
    kcUp,
    kcDown,
    kcLeft,
    kcRight,
    kcHome,
    kcEnd,
    kcSpace
  );

  TKeyResult = record
    Code: TKeyCode;
    Ch: char;
  end;

function GetKey: TKeyResult;
var
  {$IFDEF UNIX}
  c, c2, c3: char;
  {$ELSE}
  K: TKeyEvent;
  {$ENDIF}
begin
  Result.Code := kcNone;
  Result.Ch := #0;

  {$IFDEF UNIX}
  EnableRawMode;
  try
    c := ReadKeyRaw;
    case c of
      #27: // Escape sequence
      begin
        c2 := ReadKeyRaw;
        if c2 = '[' then
        begin
          c3 := ReadKeyRaw;
          case c3 of
            'A': Result.Code := kcUp;
            'B': Result.Code := kcDown;
            'C': Result.Code := kcRight;
            'D': Result.Code := kcLeft;
            'H': Result.Code := kcHome;
            'F': Result.Code := kcEnd;
            '3': begin
              ReadKeyRaw; // consume '~'
              Result.Code := kcDelete;
            end;
          else
            Result.Code := kcEscape;
          end;
        end
        else
          Result.Code := kcEscape;
      end;
      #9: Result.Code := kcTab;
      #10, #13: Result.Code := kcEnter;
      #127, #8: Result.Code := kcBackspace;
      #32: Result.Code := kcSpace;
      #33..#126:
      begin
        Result.Code := kcChar;
        Result.Ch := c;
      end;
    else
      Result.Code := kcChar;
      Result.Ch := c;
    end;
  finally
    // Don't disable here - caller controls when to disable
  end;
  {$ELSE}
  if not KeyboardInitialized then
  begin
    InitKeyboard;
    KeyboardInitialized := true;
  end;
  K := GetKeyEvent;
  K := TranslateKeyEvent(K);
  case GetKeyEventCode(K) of
    kbUp: Result.Code := kcUp;
    kbDown: Result.Code := kcDown;
    kbLeft: Result.Code := kcLeft;
    kbRight: Result.Code := kcRight;
    kbEnter: Result.Code := kcEnter;
    kbEsc: Result.Code := kcEscape;
    kbTab: Result.Code := kcTab;
    kbBack: Result.Code := kcBackspace;
    kbDel: Result.Code := kcDelete;
    kbHome: Result.Code := kcHome;
    kbEnd: Result.Code := kcEnd;
  else
    begin
      Result.Code := kcChar;
      Result.Ch := GetKeyEventChar(K);
      if Result.Ch = ' ' then
        Result.Code := kcSpace;
    end;
  end;
  {$ENDIF}
end;

procedure FinishKeyInput;
begin
  {$IFDEF UNIX}
  DisableRawMode;
  {$ENDIF}
end;

{ Helper functions }

function AnsiColor(const AFg: string; const ABold: boolean = false): string;
var
  Color: THtmlColor;
  BoldCode: string;
begin
  Color := THtmlColor.FromTailwind(AFg);
  if ABold then
    BoldCode := '1;'
  else
    BoldCode := '';
  Result := ESC + '[' + BoldCode + Format('38;2;%d;%d;%d', [Color.Red, Color.Green, Color.Blue]) + 'm';
end;

function AnsiBg(const ABg: string): string;
var
  Color: THtmlColor;
begin
  Color := THtmlColor.FromTailwind(ABg);
  Result := ESC + '[' + Format('48;2;%d;%d;%d', [Color.Red, Color.Green, Color.Blue]) + 'm';
end;

procedure ClearLine;
begin
  write(ESC + '[2K' + #13);
end;

procedure MoveCursorUp(const ALines: integer = 1);
begin
  if ALines > 0 then
    write(ESC + '[' + IntToStr(ALines) + 'A');
end;

procedure HideCursor;
begin
  write(ESC + '[?25l');
end;

procedure ShowCursor;
begin
  write(ESC + '[?25h');
end;

function RepeatStr(const S: string; Count: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + S;
end;

{ Render prompt box top }
procedure RenderPromptTop(const ALabel: string);
begin
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_TOP_LEFT + ' ' + RESET_SEQ +
    AnsiColor('white', true) + ALabel + RESET_SEQ
  );
end;

{ Render prompt box middle line }
procedure RenderPromptLine(const AContent: string = '');
begin
  write(
    AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
    ' ' + AContent
  );
end;

{ Render prompt box bottom }
procedure RenderPromptBottom;
begin
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_BOTTOM_LEFT + RESET_SEQ
  );
end;

{ Render hint text }
procedure RenderHint(const AHint: string);
begin
  if AHint <> '' then
    writeln(
      AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
      ' ' + AnsiColor('gray-500') + AHint + RESET_SEQ
    );
end;

{ Render error message }
procedure RenderError(const AError: string);
begin
  writeln(
    AnsiColor('red-500') + ' ' + BOX_VERTICAL + ' ' + CROSS_MARK + ' ' + AError + RESET_SEQ
  );
end;

{ === Text Input === }

procedure RenderTextInput(const AInput, APlaceholder: string; const ACursorPos: integer);
begin
  ClearLine;
  write(
    AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
    ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ + ' '
  );

  if AInput = '' then
    write(AnsiColor('gray-400') + APlaceholder + RESET_SEQ)
  else
    write(AInput);
end;

function text(
  const ALabel: string;
  const APlaceholder: string;
  const ADefault: string;
  const AHint: string;
  const ARequired: boolean
): string;
var
  Input: string;
  Key: TKeyResult;
  CursorPos: integer;
  Done: boolean;
  DisplayPlaceholder: string;
begin
  // Show placeholder or default
  if ADefault <> '' then
  begin
    DisplayPlaceholder := ADefault;
    Input := ADefault;
  end
  else
  begin
    DisplayPlaceholder := APlaceholder;
    Input := '';
  end;
  CursorPos := Length(Input);

  // Render prompt
  RenderPromptTop(ALabel);
  RenderHint(AHint);
  RenderTextInput(Input, DisplayPlaceholder, CursorPos);

  HideCursor;
  Done := false;
  try
    while not Done do
    begin
      Key := GetKey;
      case Key.Code of
        kcEnter:
          Done := true;
        kcBackspace:
          if CursorPos > 0 then
          begin
            Delete(Input, CursorPos, 1);
            Dec(CursorPos);
            RenderTextInput(Input, DisplayPlaceholder, CursorPos);
          end;
        kcDelete:
          if CursorPos < Length(Input) then
          begin
            Delete(Input, CursorPos + 1, 1);
            RenderTextInput(Input, DisplayPlaceholder, CursorPos);
          end;
        kcLeft:
          if CursorPos > 0 then
            Dec(CursorPos);
        kcRight:
          if CursorPos < Length(Input) then
            Inc(CursorPos);
        kcHome:
          CursorPos := 0;
        kcEnd:
          CursorPos := Length(Input);
        kcChar:
        begin
          Insert(Key.Ch, Input, CursorPos + 1);
          Inc(CursorPos);
          RenderTextInput(Input, DisplayPlaceholder, CursorPos);
        end;
        kcSpace:
        begin
          Insert(' ', Input, CursorPos + 1);
          Inc(CursorPos);
          RenderTextInput(Input, DisplayPlaceholder, CursorPos);
        end;
        kcEscape:
        begin
          Input := '';
          Done := true;
        end;
      end;
    end;
  finally
    FinishKeyInput;
    ShowCursor;
  end;

  writeln;
  Input := Trim(Input);

  // Validate required
  if ARequired and (Input = '') then
  begin
    RenderError('This field is required.');
    RenderPromptBottom;
    writeln;
    Result := text(ALabel, APlaceholder, ADefault, AHint, ARequired);
    Exit;
  end;

  RenderPromptBottom;
  Result := Input;
end;

{ === Suggest (Autocomplete) === }

function suggest(
  const ALabel: string;
  const ASuggestions: array of string;
  const APlaceholder: string;
  const ADefault: string;
  const AHint: string
): string;
var
  Input: string;
  Key: TKeyResult;
  CursorPos: integer;
  Done: boolean;
  DisplayPlaceholder: string;
  Matches: array of string;
  MatchIdx: integer;

  procedure FindMatches;
  var
    j: integer;
    LowerInput: string;
  begin
    SetLength(Matches, 0);
    if Input = '' then Exit;
    LowerInput := LowerCase(Input);
    for j := 0 to High(ASuggestions) do
      if Pos(LowerInput, LowerCase(ASuggestions[j])) = 1 then
      begin
        SetLength(Matches, Length(Matches) + 1);
        Matches[High(Matches)] := ASuggestions[j];
      end;
    MatchIdx := 0;
  end;

  procedure RenderWithSuggestion;
  var
    Suggestion: string;
  begin
    ClearLine;
    write(
      AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
      ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ + ' '
    );

    if Input = '' then
      write(AnsiColor('gray-400') + DisplayPlaceholder + RESET_SEQ)
    else
    begin
      write(Input);
      if (Length(Matches) > 0) and (MatchIdx <= High(Matches)) then
      begin
        Suggestion := Copy(Matches[MatchIdx], Length(Input) + 1, MaxInt);
        write(AnsiColor('gray-500') + Suggestion + RESET_SEQ);
      end;
    end;
  end;

begin
  if ADefault <> '' then
  begin
    DisplayPlaceholder := ADefault;
    Input := ADefault;
  end
  else
  begin
    DisplayPlaceholder := APlaceholder;
    Input := '';
  end;
  CursorPos := Length(Input);
  SetLength(Matches, 0);
  MatchIdx := 0;

  RenderPromptTop(ALabel);
  RenderHint(AHint);
  writeln(AnsiColor('gray-500') + '   TAB to autocomplete, ↑/↓ to cycle suggestions' + RESET_SEQ);
  FindMatches;
  RenderWithSuggestion;

  HideCursor;
  Done := false;
  try
    while not Done do
    begin
      Key := GetKey;
      case Key.Code of
        kcEnter:
          Done := true;
        kcTab:
          if Length(Matches) > 0 then
          begin
            Input := Matches[MatchIdx];
            CursorPos := Length(Input);
            FindMatches;
            RenderWithSuggestion;
          end;
        kcUp:
          if Length(Matches) > 0 then
          begin
            if MatchIdx > 0 then
              Dec(MatchIdx)
            else
              MatchIdx := High(Matches);
            RenderWithSuggestion;
          end;
        kcDown:
          if Length(Matches) > 0 then
          begin
            if MatchIdx < High(Matches) then
              Inc(MatchIdx)
            else
              MatchIdx := 0;
            RenderWithSuggestion;
          end;
        kcBackspace:
          if CursorPos > 0 then
          begin
            Delete(Input, CursorPos, 1);
            Dec(CursorPos);
            FindMatches;
            RenderWithSuggestion;
          end;
        kcChar:
        begin
          Insert(Key.Ch, Input, CursorPos + 1);
          Inc(CursorPos);
          FindMatches;
          RenderWithSuggestion;
        end;
        kcSpace:
        begin
          Insert(' ', Input, CursorPos + 1);
          Inc(CursorPos);
          FindMatches;
          RenderWithSuggestion;
        end;
        kcEscape:
        begin
          Input := '';
          Done := true;
        end;
      end;
    end;
  finally
    FinishKeyInput;
    ShowCursor;
  end;

  writeln;
  RenderPromptBottom;
  Result := Trim(Input);
end;

{ === Password Input === }

function password(
  const ALabel: string;
  const APlaceholder: string;
  const AHint: string
): string;
var
  Input: string;
  Key: TKeyResult;
  Done: boolean;

  procedure RenderPasswordInput;
  begin
    ClearLine;
    write(
      AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
      ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ + ' '
    );

    if Input = '' then
      write(AnsiColor('gray-400') + APlaceholder + RESET_SEQ)
    else
      write(StringOfChar('*', Length(Input)));
  end;

begin
  Input := '';

  RenderPromptTop(ALabel);
  RenderHint(AHint);
  RenderPasswordInput;

  HideCursor;
  Done := false;
  try
    while not Done do
    begin
      Key := GetKey;
      case Key.Code of
        kcEnter:
          Done := true;
        kcBackspace:
          if Length(Input) > 0 then
          begin
            Delete(Input, Length(Input), 1);
            RenderPasswordInput;
          end;
        kcChar:
        begin
          Input := Input + Key.Ch;
          RenderPasswordInput;
        end;
        kcSpace:
        begin
          Input := Input + ' ';
          RenderPasswordInput;
        end;
        kcEscape:
        begin
          Input := '';
          Done := true;
        end;
      end;
    end;
  finally
    FinishKeyInput;
    ShowCursor;
  end;

  writeln;
  RenderPromptBottom;
  Result := Input;
end;

{ === Confirm === }

function confirm(
  const ALabel: string;
  const ADefault: boolean;
  const AYes: string;
  const ANo: string;
  const AHint: string
): boolean;
var
  Input: string;
  DefaultHint: string;
begin
  if ADefault then
    DefaultHint := AnsiColor('gray-400') + '[' + AnsiColor('green-500') + 'Y' + AnsiColor('gray-400') + '/n]' + RESET_SEQ
  else
    DefaultHint := AnsiColor('gray-400') + '[y/' + AnsiColor('red-500') + 'N' + AnsiColor('gray-400') + ']' + RESET_SEQ;

  RenderPromptTop(ALabel);
  RenderHint(AHint);

  write(
    AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
    ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ +
    ' ' + DefaultHint + ' '
  );

  ReadLn(Input);
  Input := LowerCase(Trim(Input));

  if Input = '' then
    Result := ADefault
  else if (Input = 'y') or (Input = 'yes') then
    Result := true
  else if (Input = 'n') or (Input = 'no') then
    Result := false
  else
    Result := ADefault;

  // Show result
  if Result then
    writeln(AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ + ' ' + AnsiColor('green-500') + CHECK + ' ' + AYes + RESET_SEQ)
  else
    writeln(AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ + ' ' + AnsiColor('red-500') + CROSS_MARK + ' ' + ANo + RESET_SEQ);

  RenderPromptBottom;
end;

{ === Select === }

function select(
  const ALabel: string;
  const AOptions: array of string;
  const ADefault: integer;
  const AHint: string
): integer;
var
  Selected: integer;
  i: integer;
  Key: TKeyResult;
  Done: boolean;
  OptionCount: integer;

  procedure RenderOptions;
  var
    j: integer;
  begin
    // Move cursor up to redraw
    for j := 0 to OptionCount - 1 do
    begin
      MoveCursorUp(1);
      ClearLine;
    end;

    for j := 0 to High(AOptions) do
    begin
      if j = Selected then
        writeln(
          AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
          ' ' + AnsiColor('cyan-500') + CHEVRON + ' ' + RESET_SEQ +
          AnsiColor('cyan-500', true) + AOptions[j] + RESET_SEQ
        )
      else
        writeln(
          AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
          '   ' + AnsiColor('gray-400') + AOptions[j] + RESET_SEQ
        );
    end;
  end;

begin
  if Length(AOptions) = 0 then
  begin
    Result := -1;
    Exit;
  end;

  OptionCount := Length(AOptions);
  Selected := ADefault;
  if Selected < 0 then Selected := 0;
  if Selected > High(AOptions) then Selected := High(AOptions);

  RenderPromptTop(ALabel);
  RenderHint(AHint);
  writeln(AnsiColor('gray-500') + '   ↑/↓ or TAB to navigate, Enter to select' + RESET_SEQ);

  // Initial render
  for i := 0 to High(AOptions) do
  begin
    if i = Selected then
      writeln(
        AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
        ' ' + AnsiColor('cyan-500') + CHEVRON + ' ' + RESET_SEQ +
        AnsiColor('cyan-500', true) + AOptions[i] + RESET_SEQ
      )
    else
      writeln(
        AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
        '   ' + AnsiColor('gray-400') + AOptions[i] + RESET_SEQ
      );
  end;

  HideCursor;
  Done := false;
  try
    while not Done do
    begin
      Key := GetKey;
      case Key.Code of
        kcUp:
        begin
          if Selected > 0 then
            Dec(Selected)
          else
            Selected := High(AOptions);
          RenderOptions;
        end;
        kcDown, kcTab:
        begin
          if Selected < High(AOptions) then
            Inc(Selected)
          else
            Selected := 0;
          RenderOptions;
        end;
        kcEnter:
          Done := true;
        kcEscape:
        begin
          Selected := ADefault;
          Done := true;
        end;
      end;
    end;
  finally
    FinishKeyInput;
    ShowCursor;
  end;

  // Show final selection
  for i := 0 to OptionCount - 1 do
  begin
    MoveCursorUp(1);
    ClearLine;
  end;
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
    ' ' + AnsiColor('green-500') + CHECK + RESET_SEQ +
    ' ' + AnsiColor('cyan-500', true) + AOptions[Selected] + RESET_SEQ
  );

  RenderPromptBottom;
  Result := Selected;
end;

{ === Select Value === }

function selectValue(
  const ALabel: string;
  const AOptions: TPromptOptions;
  const ADefault: integer;
  const AHint: string
): string;
var
  Labels: array of string;
  i, idx: integer;
begin
  SetLength(Labels, Length(AOptions));
  for i := 0 to High(AOptions) do
    Labels[i] := AOptions[i].Label_;

  idx := select(ALabel, Labels, ADefault, AHint);

  if (idx >= 0) and (idx <= High(AOptions)) then
    Result := AOptions[idx].Value
  else
    Result := '';
end;

{ === Multi-select === }

function multiselect(
  const ALabel: string;
  const AOptions: array of string;
  const AHint: string;
  const ARequired: boolean
): TStringList;
var
  Current: integer;
  Selected: array of boolean;
  i: integer;
  Key: TKeyResult;
  Done: boolean;
  OptionCount: integer;

  procedure RenderOptions;
  var
    j: integer;
    Marker: string;
  begin
    // Move cursor up to redraw
    for j := 0 to OptionCount - 1 do
    begin
      MoveCursorUp(1);
      ClearLine;
    end;

    for j := 0 to High(AOptions) do
    begin
      if Selected[j] then
        Marker := AnsiColor('green-500') + CHECK + RESET_SEQ
      else
        Marker := AnsiColor('gray-500') + BULLET_EMPTY + RESET_SEQ;

      if j = Current then
        writeln(
          AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
          ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ +
          ' ' + Marker + ' ' +
          AnsiColor('cyan-500', true) + AOptions[j] + RESET_SEQ
        )
      else
        writeln(
          AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
          '   ' + Marker + ' ' +
          AnsiColor('gray-400') + AOptions[j] + RESET_SEQ
        );
    end;
  end;

begin
  Result := TStringList.Create;

  if Length(AOptions) = 0 then
    Exit;

  OptionCount := Length(AOptions);
  SetLength(Selected, OptionCount);
  for i := 0 to High(Selected) do
    Selected[i] := false;
  Current := 0;

  RenderPromptTop(ALabel);
  RenderHint(AHint);
  writeln(AnsiColor('gray-500') + '   ↑/↓/TAB navigate, Space toggle, Enter confirm' + RESET_SEQ);

  // Initial render
  for i := 0 to High(AOptions) do
  begin
    if i = Current then
      writeln(
        AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
        ' ' + AnsiColor('cyan-500') + CHEVRON + RESET_SEQ +
        ' ' + AnsiColor('gray-500') + BULLET_EMPTY + RESET_SEQ + ' ' +
        AnsiColor('cyan-500', true) + AOptions[i] + RESET_SEQ
      )
    else
      writeln(
        AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
        '   ' + AnsiColor('gray-500') + BULLET_EMPTY + RESET_SEQ + ' ' +
        AnsiColor('gray-400') + AOptions[i] + RESET_SEQ
      );
  end;

  HideCursor;
  Done := false;
  try
    while not Done do
    begin
      Key := GetKey;
      case Key.Code of
        kcUp:
        begin
          if Current > 0 then
            Dec(Current)
          else
            Current := High(AOptions);
          RenderOptions;
        end;
        kcDown, kcTab:
        begin
          if Current < High(AOptions) then
            Inc(Current)
          else
            Current := 0;
          RenderOptions;
        end;
        kcSpace:
        begin
          Selected[Current] := not Selected[Current];
          RenderOptions;
        end;
        kcEnter:
          Done := true;
        kcEscape:
        begin
          for i := 0 to High(Selected) do
            Selected[i] := false;
          Done := true;
        end;
      end;
    end;
  finally
    FinishKeyInput;
    ShowCursor;
  end;

  // Collect selected items
  for i := 0 to High(AOptions) do
    if Selected[i] then
      Result.Add(AOptions[i]);

  // Check required
  if ARequired and (Result.Count = 0) then
  begin
    // Clear the options display
    for i := 0 to OptionCount - 1 do
    begin
      MoveCursorUp(1);
      ClearLine;
    end;
    RenderError('Please select at least one option.');
    RenderPromptBottom;
    writeln;
    Result.Free;
    Result := multiselect(ALabel, AOptions, AHint, ARequired);
    Exit;
  end;

  // Show final selection
  for i := 0 to OptionCount - 1 do
  begin
    MoveCursorUp(1);
    ClearLine;
  end;

  if Result.Count > 0 then
  begin
    writeln(
      AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
      ' ' + AnsiColor('green-500') + CHECK + RESET_SEQ +
      ' Selected: ' + AnsiColor('cyan-500', true) + IntToStr(Result.Count) + ' item(s)' + RESET_SEQ
    );
  end
  else
    writeln(
      AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
      ' ' + AnsiColor('gray-500') + 'No items selected' + RESET_SEQ
    );

  RenderPromptBottom;
end;

{ === Pause === }

procedure pause(const AMessage: string);
begin
  writeln;
  write(AnsiColor('gray-400') + ' ' + AMessage + RESET_SEQ);
  ReadLn;
end;

{ === Informational Outputs === }

procedure note(const AMessage: string);
begin
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ +
    ' ' + AnsiColor('gray-400') + AMessage + RESET_SEQ
  );
end;

procedure intro(const ATitle: string);
begin
  writeln;
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_TOP_LEFT + BOX_HORIZONTAL + ' ' + RESET_SEQ +
    AnsiColor('cyan-500', true) + ATitle + RESET_SEQ
  );
end;

procedure outro(const AMessage: string);
begin
  writeln(
    AnsiColor('cyan-500') + ' ' + BOX_BOTTOM_LEFT + BOX_HORIZONTAL + ' ' + RESET_SEQ +
    AnsiColor('gray-400') + AMessage + RESET_SEQ
  );
  writeln;
end;

procedure alert(const AMessage: string);
var
  Padding: string;
  Width: integer;
begin
  Width := Length(AMessage) + 4;
  Padding := StringOfChar(' ', Width);

  writeln;
  writeln(AnsiBg('yellow-500') + AnsiColor('black') + Padding + RESET_SEQ);
  writeln(AnsiBg('yellow-500') + AnsiColor('black') + '  ' + AMessage + '  ' + RESET_SEQ);
  writeln(AnsiBg('yellow-500') + AnsiColor('black') + Padding + RESET_SEQ);
  writeln;
end;

{ === Spinner === }

procedure spin(const AMessage: string);
begin
  SpinnerFrame := 0;
  write(AnsiColor('cyan-500') + ' ' + SPINNER_FRAMES[SpinnerFrame] + RESET_SEQ + ' ' + AMessage);
end;

procedure spinStop(const ASuccess: boolean; const AMessage: string);
var
  Icon: string;
begin
  ClearLine;

  if ASuccess then
    Icon := AnsiColor('green-500') + CHECK + RESET_SEQ
  else
    Icon := AnsiColor('red-500') + CROSS_MARK + RESET_SEQ;

  if AMessage <> '' then
    writeln(' ' + Icon + ' ' + AMessage)
  else
    writeln(' ' + Icon);
end;

{ === Progress Bar === }

constructor TProgressBar.Create(const ALabel: string; const ATotal: integer; const AWidth: integer);
begin
  inherited Create;
  FLabel := ALabel;
  FTotal := ATotal;
  FCurrent := 0;
  FWidth := AWidth;

  // Initial render
  write(AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ + ' ' + FLabel + ' ');
  write(AnsiColor('gray-600') + RepeatStr(#$E2#$96#$91, FWidth) + RESET_SEQ);  // ░
  write(' 0%');
end;

procedure TProgressBar.Advance(const AStep: integer);
var
  Filled: integer;
  Percent: integer;
begin
  FCurrent := FCurrent + AStep;
  if FCurrent > FTotal then FCurrent := FTotal;

  Percent := Round((FCurrent / FTotal) * 100);
  Filled := Round((FCurrent / FTotal) * FWidth);

  ClearLine;
  write(AnsiColor('cyan-500') + ' ' + BOX_VERTICAL + RESET_SEQ + ' ' + FLabel + ' ');
  write(AnsiColor('cyan-500') + RepeatStr(#$E2#$96#$88, Filled) + RESET_SEQ);  // █
  write(AnsiColor('gray-600') + RepeatStr(#$E2#$96#$91, FWidth - Filled) + RESET_SEQ);  // ░
  write(' ' + IntToStr(Percent) + '%');
end;

procedure TProgressBar.Finish;
begin
  FCurrent := FTotal;
  Advance(0);
  writeln;
  writeln(AnsiColor('green-500') + ' ' + CHECK + RESET_SEQ + ' Done!');
end;

function progress(const ALabel: string; const ATotal: integer; const AWidth: integer): TProgressBar;
begin
  Result := TProgressBar.Create(ALabel, ATotal, AWidth);
end;

{ === Table === }

procedure table(const AHeaders: array of string; const ARows: array of TStringArray);
var
  ColWidths: array of integer;
  i, j, w: integer;
  Line: string;
  CellValue: string;
begin
  if Length(AHeaders) = 0 then Exit;

  // Calculate column widths
  SetLength(ColWidths, Length(AHeaders));
  for i := 0 to High(AHeaders) do
    ColWidths[i] := Length(AHeaders[i]);

  for i := 0 to High(ARows) do
    for j := 0 to High(ARows[i]) do
      if j <= High(ColWidths) then
      begin
        w := Length(ARows[i][j]);
        if w > ColWidths[j] then
          ColWidths[j] := w;
      end;

  // Top border
  Line := BOX_TOP_LEFT;
  for i := 0 to High(ColWidths) do
  begin
    Line := Line + RepeatStr(BOX_HORIZONTAL, ColWidths[i] + 2);
    if i < High(ColWidths) then
      Line := Line + BOX_T_DOWN
    else
      Line := Line + BOX_TOP_RIGHT;
  end;
  writeln(AnsiColor('gray-600') + Line + RESET_SEQ);

  // Header row
  write(AnsiColor('gray-600') + BOX_VERTICAL + RESET_SEQ);
  for i := 0 to High(AHeaders) do
  begin
    write(' ' + AnsiColor('white', true) +
      AHeaders[i] + StringOfChar(' ', ColWidths[i] - Length(AHeaders[i])) +
      RESET_SEQ + ' ' + AnsiColor('gray-600') + BOX_VERTICAL + RESET_SEQ);
  end;
  writeln;

  // Header separator
  Line := BOX_T_RIGHT;
  for i := 0 to High(ColWidths) do
  begin
    Line := Line + RepeatStr(BOX_HORIZONTAL, ColWidths[i] + 2);
    if i < High(ColWidths) then
      Line := Line + BOX_CROSS
    else
      Line := Line + BOX_T_LEFT;
  end;
  writeln(AnsiColor('gray-600') + Line + RESET_SEQ);

  // Data rows
  for i := 0 to High(ARows) do
  begin
    write(AnsiColor('gray-600') + BOX_VERTICAL + RESET_SEQ);
    for j := 0 to High(ColWidths) do
    begin
      if j <= High(ARows[i]) then
        CellValue := ARows[i][j]
      else
        CellValue := '';
      write(' ' + CellValue + StringOfChar(' ', ColWidths[j] - Length(CellValue)) + ' ');
      write(AnsiColor('gray-600') + BOX_VERTICAL + RESET_SEQ);
    end;
    writeln;
  end;

  // Bottom border
  Line := BOX_BOTTOM_LEFT;
  for i := 0 to High(ColWidths) do
  begin
    Line := Line + RepeatStr(BOX_HORIZONTAL, ColWidths[i] + 2);
    if i < High(ColWidths) then
      Line := Line + BOX_T_UP
    else
      Line := Line + BOX_BOTTOM_RIGHT;
  end;
  writeln(AnsiColor('gray-600') + Line + RESET_SEQ);
end;

end.

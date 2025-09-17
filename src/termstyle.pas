unit termstyle;

{$mode objfpc}{$H+}

interface

function FormatText(const S: string): string;

// Helper printing functions
procedure PrintError(const Msg: string);
procedure PrintSuccess(const Msg: string);
procedure PrintWarn(const Msg: string);
procedure PrintInfo(const Msg: string);
procedure PrintBanner(const OpenTag, CloseTag, Title: string);
function Prompt(const Prefix: string): string;

implementation

uses
  Classes, SysUtils;

type
  TStyleStack = class
  private
    FStack: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(const Code: string);
    function Pop: string;
    function Rebuild: string;
    function Current: string;
  end;

constructor TStyleStack.Create;
begin
  FStack := TStringList.Create;
end;

destructor TStyleStack.Destroy;
begin
  FStack.Free;
  inherited;
end;

procedure TStyleStack.Push(const Code: string);
begin
  if Code <> '' then
    FStack.Add(Code);
end;

function TStyleStack.Pop: string;
begin
  if FStack.Count > 0 then
  begin
    Result := FStack[FStack.Count - 1];
    FStack.Delete(FStack.Count - 1);
  end
  else
    Result := '';
end;

function TStyleStack.Rebuild: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to FStack.Count - 1 do
    Result := Result + FStack[I];
end;

function TStyleStack.Current: string;
begin
  if FStack.Count > 0 then
    Result := FStack[FStack.Count - 1]
  else
    Result := '';
end;

function AnsiCodeForToken(const Token: string): string;
begin
  case LowerCase(Token) of
    // attributes
    'reset': Result := #27'[0m';
    'bold': Result := #27'[1m';
    'dim': Result := #27'[2m';
    'italic': Result := #27'[3m';
    'underline': Result := #27'[4m';
    'blink': Result := #27'[5m';
    'reversed': Result := #27'[7m';
    'hidden': Result := #27'[8m';
    'strikethrough': Result := #27'[9m';

    // foreground colors
    'black': Result := #27'[30m';
    'red': Result := #27'[31m';
    'green': Result := #27'[32m';
    'yellow': Result := #27'[33m';
    'blue': Result := #27'[34m';
    'magenta': Result := #27'[35m';
    'cyan': Result := #27'[36m';
    'white': Result := #27'[37m';

    // bright foregrounds
    'bright_black': Result := #27'[90m';
    'bright_red': Result := #27'[91m';
    'bright_green': Result := #27'[92m';
    'bright_yellow': Result := #27'[93m';
    'bright_blue': Result := #27'[94m';
    'bright_magenta': Result := #27'[95m';
    'bright_cyan': Result := #27'[96m';
    'bright_white': Result := #27'[97m';

    // backgrounds
    'bg:black': Result := #27'[40m';
    'bg:red': Result := #27'[41m';
    'bg:green': Result := #27'[42m';
    'bg:yellow': Result := #27'[43m';
    'bg:blue': Result := #27'[44m';
    'bg:magenta': Result := #27'[45m';
    'bg:cyan': Result := #27'[46m';
    'bg:white': Result := #27'[47m';

    // bright backgrounds
    'bg:bright_black': Result := #27'[100m';
    'bg:bright_red': Result := #27'[101m';
    'bg:bright_green': Result := #27'[102m';
    'bg:bright_yellow': Result := #27'[103m';
    'bg:bright_blue': Result := #27'[104m';
    'bg:bright_magenta': Result := #27'[105m';
    'bg:bright_cyan': Result := #27'[106m';
    'bg:bright_white': Result := #27'[107m';
    else
      Result := '';
  end;
end;

function SplitTokens(const S: string): TStringList;
var
  Part: string;
  i:    integer;
begin
  Result := TStringList.Create;
  Part := '';
  for i := 1 to Length(S) do
    if S[i] = ' ' then
    begin
      if Part <> '' then
      begin
        Result.Add(Part);
        Part := '';
      end;
    end
    else
      Part := Part + S[i];
  if Part <> '' then
    Result.Add(Part);
end;

function FormatText(const S: string): string;
type
  TTokenType = (ttText, ttOpenTag, ttCloseTag);

  TToken = record
    TokenType: TTokenType;
    Value: string;
  end;

  TTagStackItem = record
    Codes: TStringList;
  end;
var
  Tokens: array of TToken;
  I, StartPos, Len, TokenCount: integer;
  ResultStr: string;
  StyleStack: array of TTagStackItem; // stack of pushed tag codes
  Token: TToken;
  TagTokens, CodeList: TStringList;
  Code: string;

  procedure AddToken(TokenType: TTokenType; const Value: string);
  begin
    Inc(TokenCount);
    SetLength(Tokens, TokenCount);
    Tokens[TokenCount - 1].TokenType := TokenType;
    Tokens[TokenCount - 1].Value := Value;
  end;

  procedure PushTagCodes(Codes: TStringList);
  var
    Item: TTagStackItem;
  begin
    Item.Codes := TStringList.Create;
    Item.Codes.Assign(Codes);
    SetLength(StyleStack, Length(StyleStack) + 1);
    StyleStack[High(StyleStack)] := Item;
  end;

  procedure PopTagCodes;
  var
    Item: TTagStackItem;
    J:    integer;
    RebuildStr: string;
  begin
    if Length(StyleStack) = 0 then
    begin
      ResultStr := ResultStr + #27'[0m';
      Exit;
    end;

    Item := StyleStack[High(StyleStack)];
    for J := 0 to Item.Codes.Count - 1 do
      Item.Codes[J] := ''; // optional clear
    Item.Codes.Free;
    SetLength(StyleStack, Length(StyleStack) - 1);

    // reset terminal
    ResultStr := ResultStr + #27'[0m';

    // rebuild remaining styles in stack
    RebuildStr := '';
    for J := 0 to High(StyleStack) do
      RebuildStr := RebuildStr + StyleStack[J].Codes.Text.Replace(sLineBreak, '');
    ResultStr := ResultStr + RebuildStr;
  end;

begin
  // --- Step 1: Tokenize input ---
  TokenCount := 0;
  StartPos := 1;
  Len := Length(S);
  while StartPos <= Len do
  begin
    if S[StartPos] = '<' then
    begin
      I := Pos('>', S, StartPos);
      if I > 0 then
      begin
        if (StartPos + 1 <= I - 1) and (S[StartPos + 1] = '/') then
          AddToken(ttCloseTag, Copy(S, StartPos + 2, I - StartPos - 2))
        else
          AddToken(ttOpenTag, Copy(S, StartPos + 1, I - StartPos - 1));
        StartPos := I + 1;
        Continue;
      end;
    end;

    I := StartPos;
    while (I <= Len) and (S[I] <> '<') do
      Inc(I);
    AddToken(ttText, Copy(S, StartPos, I - StartPos));
    StartPos := I;
  end;

  // --- Step 2: Process tokens ---
  ResultStr := '';
  SetLength(StyleStack, 0);

  for I := 0 to High(Tokens) do
  begin
    Token := Tokens[I];
    case Token.TokenType of
      ttText:
        ResultStr := ResultStr + Token.Value;

      ttOpenTag:
      begin
        TagTokens := SplitTokens(Token.Value);
        try
          CodeList := TStringList.Create;
          try
            for Code in TagTokens do
            begin
              CodeList.Add(AnsiCodeForToken(Code));
              ResultStr := ResultStr + AnsiCodeForToken(Code);
            end;
            PushTagCodes(CodeList);
          finally
            CodeList.Free;
          end;
        finally
          TagTokens.Free;
        end;
      end;

      ttCloseTag:
        PopTagCodes;
    end;
  end;

  Result := ResultStr + #27'[0m';
end;

{ === Helper Functions === }

procedure PrintError(const Msg: string);
begin
  Writeln(FormatText('<bright_white bg:bright_red bold> ERROR </bright_white> ' + Msg));
end;

procedure PrintSuccess(const Msg: string);
begin
  Writeln(FormatText('<bright_white bg:bright_green bold> SUCCESS </bright_white> '
    + Msg));
end;

procedure PrintWarn(const Msg: string);
begin
  Writeln(FormatText('<bright_white bg:bright_yellow bold> WARNING </bright_white> '
    + Msg));
end;

procedure PrintInfo(const Msg: string);
begin
  Writeln(FormatText('<bright_white bg:bright_blue bold> INFO </bright_white> ' + Msg));
end;

procedure PrintBanner(const OpenTag, CloseTag, Title: string);
var
  TotalWidth, Padding, i: integer;
  Line: string;
begin
  TotalWidth := 50; // total banner width
  if Length(Title) >= TotalWidth then
    Padding := 0
  else
    Padding := (TotalWidth - Length(Title)) div 2;

  // Top line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  Writeln(FormatText(OpenTag + Line + CloseTag));

  // Title line, centered
  Line := '';
  for i := 1 to Padding do
    Line := Line + ' ';
  Line := Line + Title;
  while Length(Line) < TotalWidth do
    Line := Line + ' ';
  Writeln(FormatText(OpenTag + Line + CloseTag));

  // Bottom line (spaces with background)
  Line := '';
  for i := 1 to TotalWidth do
    Line := Line + ' ';
  Writeln(FormatText(OpenTag + Line + CloseTag));
end;


function Prompt(const Prefix: string): string;
begin
  Write(FormatText('<bright_magenta>' + Prefix + '></bright_magenta> '));
  ReadLn(Result);
end;

end.

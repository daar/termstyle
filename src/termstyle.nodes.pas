unit TermStyle.Nodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Contnrs,
  DOM,
  StrUtils,
  SysUtils,
  TermStyle.Color;

type
  { TClassEnumerator }

  TClassEnumerator = class
  private
    FText: string;
    FIndex: integer;
    FCurrent: string;
  public
    constructor Create(const AText: string);
    function MoveNext: boolean;
    property Current: string read FCurrent;
  end;

  { THtmlNode }

  THtmlNode = class
  private
    FClasses: string;
    FChildren: TObjectList; // owns its children

    procedure AnsiCodeForClass(const AClass: string);
    procedure ParseBoxClass(const AClass: string);
  public
    Style: TTextStyle;

    constructor Create(const AClass: string = '');
    destructor Destroy; override;

    procedure AddChild(Node: THtmlNode);
    property Children: TObjectList read FChildren;

    function Render: string; virtual;

    function GetEnumerator: TClassEnumerator;
  end;

  { THtmlBody }

  THtmlBody = class(THtmlNode)
  public
    function Render: string; override;
  end;

  { THtmlText }

  THtmlText = class(THtmlNode)
  public
    Text: string;
    constructor Create(const AClass, AText: string);
    function Render: string; override;
  end;

  { THtmlDiv }

  THtmlDiv = class(THtmlNode);

  { THtmlSpan }

  THtmlSpan = class(THtmlNode);

  { THtmlP }

  THtmlP = class(THtmlNode);

  { THtmlA }

  THtmlA = class(THtmlNode)
  public
    Href: string;
    constructor Create(const AClass, AHref: string);
    function Render: string; override;
  end;

  { THtmlS }

  THtmlS = class(THtmlNode)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlB }

  THtmlB = class(THtmlNode)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlStrong }

  THtmlStrong = class(THtmlNode)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlI }

  THtmlI = class(THtmlNode)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlEm }

  THtmlEm = class(THtmlNode)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlUl }

  THtmlUl = class(THtmlNode)
  public
    constructor Create(const AClass: string);
    function Render: string; override;
  end;

  { THtmlOl }

  THtmlOl = class(THtmlUl)
  public
    constructor Create(const AClass: string);
  end;

  { THtmlLi }

  THtmlLi = class(THtmlNode);

function CreateNodeFromElement(Node: TDOMNode): THtmlNode;
procedure TraverseNode(Node: TDOMNode; ParentHtmlNode: THtmlNode);

implementation

uses
  Math;

function CapitalizeWords(const S: string): string;
var
  parts: TStringList;
  i:     integer;
begin
  parts := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(S), parts); // crude split on spaces
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then
        parts[i] := UpperCase(parts[i][1]) + LowerCase(Copy(parts[i], 2, MaxInt));
    Result := Trim(StringReplace(parts.Text, sLineBreak, ' ', [rfReplaceAll]));
  finally
    parts.Free;
  end;
end;

function ToSnakeCase(const S: string): string;
var
  i:  integer;
  ch: char;
  sb: string;
begin
  sb := '';
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    if ch in ['A'..'Z'] then
      sb += LowerCase(ch)
    else if ch in ['a'..'z', '0'..'9'] then
      sb += ch
    else if ch = ' ' then
      sb += '_'
    else
      sb += '_';
  end;
  // collapse multiple underscores
  Result := sb;
  while Pos('__', Result) > 0 do
    Result := StringReplace(Result, '__', '_', [rfReplaceAll]);
  // trim underscores
  Result := Trim(Result);
  if (Result <> '') and (Result[1] = '_') then Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '_') then
    Delete(Result, Length(Result), 1);
end;

function HtmlDecode(const S: string): string;
const
  Entities: array[0..5] of record
      name: string;
      char: string;
      end
  = (
    (name: '&amp;'; char: '&'),
    (name: '&lt;'; char: '<'),
    (name: '&gt;'; char: '>'),
    (name: '&quot;'; char: '"'),
    (name: '&apos;'; char: ''''),
    (name: '&nbsp;'; char: ' ')
    );
var
  ResultStr: string;
  i: integer;
begin
  ResultStr := S;
  for i := Low(Entities) to High(Entities) do
    ResultStr := StringReplace(ResultStr, Entities[i].name, Entities[i].char,
      [rfReplaceAll]);
  Result := ResultStr;
end;

{ TClassEnumerator }

constructor TClassEnumerator.Create(const AText: string);
begin
  FText := AText;
  FIndex := 1;
end;

function TClassEnumerator.MoveNext: boolean;
begin
  FCurrent := ExtractWord(FIndex, FText, [' ']);
  Result := FCurrent <> '';
  if Result then
    Inc(FIndex);
end;

{ THtmlNode }

constructor THtmlNode.Create(const AClass: string);
var
  SingleClass: string;
begin
  inherited Create;

  FClasses := AClass;
  FChildren := TObjectList.Create(True);

  // create style object for this node
  Style := TTextStyle.Create;

  //TODO: optimization possible to parse all classes more effectively
  //      possibly replace these functions by a style class??
  //TODO: move the following code to TTextStyle.Create
  for SingleClass in Self do
  begin
    AnsiCodeForClass(lowercase(SingleClass));

    ParseBoxClass(lowercase(SingleClass));
  end;
end;

destructor THtmlNode.Destroy;
begin
  Style.Free;
  FChildren.Free;
  inherited Destroy;
end;

procedure THtmlNode.AddChild(Node: THtmlNode);
begin
  FChildren.Add(Node);
end;

procedure THtmlNode.AnsiCodeForClass(const AClass: string);
var
  ColorName: string;
begin
  case AClass of
    // attributes
    'font-bold': include(Style.Attrs, taBold);
    'font-normal': exclude(Style.Attrs, taBold);
    'font-light': include(Style.Attrs, taLight);
    'italic': include(Style.Attrs, taItalic);
    'underline': include(Style.Attrs, taUnderline);
    'animate-pulse': include(Style.Attrs, taBlink);
    'invert': include(Style.Attrs, taInverse);
    'sr-only': include(Style.Attrs, taHidden);
    'invisible': include(Style.Attrs, taHidden);
    'line-through': include(Style.Attrs, taStrike);

    // transforms
    'uppercase': Style.Transform := ttUppercase;
    'lowercase': Style.Transform := ttLowercase;
    'capitalize': Style.Transform := ttCapitalize;
    'snakecase': Style.Transform := ttSnakeCase;
    'normal-case': Style.Transform := ttNormalCase;

    else
      // Check for colors
      if Copy(AClass, 1, 5) = 'text-' then
      begin
        ColorName := Copy(AClass, 6, Length(AClass) - 5);
        Style.FG := THtmlColor.FromTailwind(ColorName); // last text-* wins
      end
      else if Copy(AClass, 1, 3) = 'bg-' then
      begin
        ColorName := Copy(AClass, 4, Length(AClass) - 3);
        Style.BG := THtmlColor.FromTailwind(ColorName); // last bg-* wins
      end;
  end;
end;

type
  TBoxSide = (bsTop, bsBottom, bsLeft, bsRight);
  TBoxSides = set of TBoxSide;
  TBoxTarget = (btPadding, btMargin);

  TBoxRule = record
    Prefix: string;
    Target: TBoxTarget;
    Sides: TBoxSides;
  end;

const
  // Map prefix → sides and target
  BoxRules: array[0..13] of TBoxRule = (
    (Prefix: 'p-'; Target: btPadding; Sides: [bsTop, bsBottom, bsLeft, bsRight]),
    (Prefix: 'pt-'; Target: btPadding; Sides: [bsTop]),
    (Prefix: 'pr-'; Target: btPadding; Sides: [bsRight]),
    (Prefix: 'pb-'; Target: btPadding; Sides: [bsBottom]),
    (Prefix: 'pl-'; Target: btPadding; Sides: [bsLeft]),
    (Prefix: 'px-'; Target: btPadding; Sides: [bsLeft, bsRight]),
    (Prefix: 'py-'; Target: btPadding; Sides: [bsTop, bsBottom]),

    (Prefix: 'm-'; Target: btMargin; Sides: [bsTop, bsBottom, bsLeft, bsRight]),
    (Prefix: 'mt-'; Target: btMargin; Sides: [bsTop]),
    (Prefix: 'mr-'; Target: btMargin; Sides: [bsRight]),
    (Prefix: 'mb-'; Target: btMargin; Sides: [bsBottom]),
    (Prefix: 'ml-'; Target: btMargin; Sides: [bsLeft]),
    (Prefix: 'mx-'; Target: btMargin; Sides: [bsLeft, bsRight]),
    (Prefix: 'my-'; Target: btMargin; Sides: [bsTop, bsBottom])
    );

procedure THtmlNode.ParseBoxClass(const AClass: string);
var
  v:    integer;
  vstr: string;
  Box:  PBoxSpacing;
  Rule: ^TBoxRule;
begin
  // Guard against short strings
  if Length(AClass) < 2 then Exit;

  // Quick prefix matching
  Rule := nil;
  case AClass[1] of
    'p':  // padding prefixes
      case AClass[2] of
        '-': Rule := @BoxRules[0];
        't': Rule := @BoxRules[1];
        'r': Rule := @BoxRules[2];
        'b': Rule := @BoxRules[3];
        'l': Rule := @BoxRules[4];
        'x': Rule := @BoxRules[5];
        'y': Rule := @BoxRules[6];
      end;
    'm':  // margin prefixes
      case AClass[2] of
        '-': Rule := @BoxRules[7];
        't': Rule := @BoxRules[8];
        'r': Rule := @BoxRules[9];
        'b': Rule := @BoxRules[10];
        'l': Rule := @BoxRules[11];
        'x': Rule := @BoxRules[12];
        'y': Rule := @BoxRules[13];
      end;
  end;

  if Rule = nil then Exit;

  vstr := Copy(AClass, Length(Rule^.Prefix) + 1, MaxInt);
  if not TryStrToInt(vstr, v) then Exit;

  if Rule^.Target = btPadding then
    Box := @Style.Padding
  else
    Box := @Style.Margin;

  if bsTop in Rule^.Sides then Box^.Top := v;
  if bsBottom in Rule^.Sides then Box^.Bottom := v;
  if bsLeft in Rule^.Sides then Box^.Left := v;
  if bsRight in Rule^.Sides then Box^.Right := v;
end;

function StripAnsi(const S: string): string;
var
  i:     integer;
  InSeq: boolean;
begin
  Result := '';
  InSeq := False;
  i := 1;
  while i <= Length(S) do
    if InSeq then
    begin
      // Skip characters until 'm' is found
      if S[i] = 'm' then
        InSeq := False;
      Inc(i);
    end
    else if S[i] = ESC then
    begin
      InSeq := True;
      Inc(i);
    end
    else
    begin
      Result := Result + S[i];
      Inc(i);
    end;
end;

function THtmlNode.Render: string;
var
  i: integer;
  Inner, Line, PaddedLine: string;
  InnerLines: TStringArray;
  ContentWidth, PaddedWidth, w: integer;
  PaddedLines: array of string;
  ParentAnsi: string;
begin
  // Render children
  Inner := '';
  for i := 0 to Children.Count - 1 do
    Inner += THtmlNode(Children[i]).Render;

  // Split into lines and compute widest content width (no ANSI)
  InnerLines := Inner.Split([sLineBreak]);
  ContentWidth := 0;
  for i := 0 to High(InnerLines) do
  begin
    w := Length(StripAnsi(InnerLines[i]));
    if w > ContentWidth then ContentWidth := w;
  end;

  // Build padded lines array
  SetLength(PaddedLines, 0);

  // Top padding (each line uses this node's style)
  for i := 1 to Style.Padding.Top do
  begin
    SetLength(PaddedLines, Length(PaddedLines) + 1);
    PaddedLines[High(PaddedLines)] :=
      Style.ToAnsi + StringOfChar(' ', ContentWidth + Style.Padding.Left +
      Style.Padding.Right) + RESET_SEQ;
  end;

  // Content lines with left/right padding
  for i := 0 to High(InnerLines) do
  begin
    PaddedLine := '';

    // LEFT padding: only emit node style if left padding > 0
    if Style.Padding.Left > 0 then
    begin
      PaddedLine +=
        Style.ToAnsi + 
        StringOfChar(' ', Style.Padding.Left);

        // Append the already-rendered child line (may contain its own ANSI)
      PaddedLine += InnerLines[i];
    end
    else
      PaddedLine := Style.ToAnsi + InnerLines[i] + RESET_SEQ;

    // RIGHT padding: re-apply node style only if right padding > 0
    if Style.Padding.Right > 0 then
      PaddedLine += 
        Style.ToAnsi + 
        StringOfChar(' ', Style.Padding.Right);

    // Close node style only when we opened it for padding
    if (Style.Padding.Left > 0) or (Style.Padding.Right > 0) then
      PaddedLine += RESET_SEQ;

    // Always add a newline per content line (previous bug: it was conditional)
    SetLength(PaddedLines, Length(PaddedLines) + 1);
    PaddedLines[High(PaddedLines)] := PaddedLine;
  end;

  // Bottom padding
  for i := 1 to Style.Padding.Bottom do
  begin
    SetLength(PaddedLines, Length(PaddedLines) + 1);
    PaddedLines[High(PaddedLines)] :=
      Style.ToAnsi + 
      StringOfChar(' ', ContentWidth + Style.Padding.Left + Style.Padding.Right) + 
      RESET_SEQ;
  end;

  // Apply margin using parent style (guard if parent is nil)
  if Assigned(Style.Parent) then
    ParentAnsi := Style.Parent.ToAnsi
  else
    ParentAnsi := '';

  // compute the padded content width (strip ANSI from each padded line)
  PaddedWidth := 0;
  for i := 0 to High(PaddedLines) do
  begin
    w := Length(StripAnsi(PaddedLines[i]));
    if w > PaddedWidth then PaddedWidth := w;
  end;

  // Build final Result: top margin lines, padded lines with left/right margin, bottom margin
  Result := '';

  // top margin
  for i := 1 to Style.Margin.Top do
    Result += 
      ParentAnsi + 
      StringOfChar(' ', Style.Margin.Left + PaddedWidth + Style.Margin.Right) + 
      RESET_SEQ + 
      sLineBreak;

  // content lines with margins
  for i := 0 to High(PaddedLines) do
  begin
    Line := '';

    if Style.Margin.Left > 0 then
      Line += 
        ParentAnsi + 
        StringOfChar(' ', Style.Margin.Left);

    Line += PaddedLines[i];

    if Style.Margin.Right > 0 then
      Line += 
        ParentAnsi + 
        StringOfChar(' ', Style.Margin.Right) + 
        RESET_SEQ;

    Result += Line + sLineBreak;
  end;

  // bottom margin
  for i := 1 to Style.Margin.Bottom do
    Result += 
      ParentAnsi + 
      StringOfChar(' ', Style.Margin.Left + PaddedWidth + Style.Margin.Right) + 
      RESET_SEQ + 
      sLineBreak;

  // Trim final trailing newline (if present)
  if (Length(Result) >= Length(sLineBreak)) and
    (Copy(Result, Length(Result) - Length(sLineBreak) + 1, Length(sLineBreak)) =
    sLineBreak) then
    Delete(Result, Length(Result) - Length(sLineBreak) + 1, Length(sLineBreak));
end;

function THtmlNode.GetEnumerator: TClassEnumerator;
begin
  Result := TClassEnumerator.Create(FClasses);
end;

{ THtmlBody }

function THtmlBody.Render: string;
var
  i: integer;
begin
  Result := '';

  // Render each child recursively
  for i := 0 to Children.Count - 1 do
    Result += THtmlNode(Children[i]).Render;
end;

{ THtmlA }

constructor THtmlA.Create(const AClass, AHref: string);
begin
  inherited Create(AClass);

  Href := AHref;

  Style.FG := THtmlColor.FromTailwind('blue-500');
end;

function THtmlA.Render: string;
var
  childRendered: string = '';
  i: integer;
begin
  // Render all children recursively
  for i := 0 to Children.Count - 1 do
    childRendered += THtmlNode(Children[i]).Render;

  // Wrap the rendered children in the OSC 8 escape sequence for hyperlinks
  Result :=
    #27']8;;' + Href + #7 +  // start hyperlink
    Style.ToAnsi + 
    childRendered +          // rendered children (text + styled content)
    RESET_SEQ + 
    #27']8;;'#7;             // end hyperlink
end;

{ THtmlS }

constructor THtmlS.Create(const AClass: string);
begin
  inherited Create(AClass);

  Include(Style.Attrs, taStrike);
end;

constructor THtmlB.Create(const AClass: string);
begin
  inherited Create(AClass);

  Include(Style.Attrs, taBold);
end;

{ THtmlStrong }

constructor THtmlStrong.Create(const AClass: string);
begin
  inherited Create(AClass);

  Include(Style.Attrs, taBold);
end;

{ THtmlI }

constructor THtmlI.Create(const AClass: string);
begin
  inherited Create(AClass);

  Include(Style.Attrs, taItalic);
end;

{ THtmlEm }

constructor THtmlEm.Create(const AClass: string);
begin
  inherited Create(AClass);

  Include(Style.Attrs, taItalic);
end;

function ParseListStyle(const AClass: string; const ADefault: TListStyle): TListStyle;
var
  ClassLower: string;
begin
  ClassLower := LowerCase(AClass);

  if Pos('list-disc', ClassLower) > 0 then
    Result := lsDisc
  else if Pos('list-decimal', ClassLower) > 0 then
    Result := lsDecimal
  else
    Result := ADefault;
end;

{ THtmlUl }

constructor THtmlUl.Create(const AClass: string);
begin
  inherited Create(AClass);

  Style.ListStyle := ParseListStyle(AClass, lsDisc);
end;

function THtmlUl.Render: string;
var
  i:      integer;
  LiNode: THtmlLi;
  Number: integer;
  Prefix: string;
begin
  Result := '';
  Number := 1; // Start numbering from 1

  // Determine prefix based on ListStyle
  case Style.ListStyle of
    lsDisc: Prefix := '• ';
    lsDecimal: Prefix := '';
    else
      Prefix := '';
  end;

  for i := 0 to Children.Count - 1 do
    if Children[i] is THtmlLi then
    begin
      LiNode := THtmlLi(Children[i]);

      // Set dynamic prefix for this LI
      if LiNode.Style.ListStyle = lsDecimal then
        Result +=
          LiNode.Style.ToAnsi + 
          IntToStr(Number) + '. ' +
          THtmlNode(LiNode.Children[0]).Render + 
          sLineBreak
      else
        Result +=
          LiNode.Style.ToAnsi + 
          Prefix +
          THtmlNode(LiNode.Children[0]).Render + 
          sLineBreak;

      Inc(Number);
    end
    else
      raise Exception.Create('List tags can only contain <li> elements');
end;

{ THtmlOl }

constructor THtmlOl.Create(const AClass: string);
begin
  inherited Create(AClass);

  Style.ListStyle := ParseListStyle(AClass, lsDecimal);
end;

{ THtmlText }

constructor THtmlText.Create(const AClass, AText: string);
begin
  inherited Create(AClass);

  Text := HtmlDecode(AText);
end;

function THtmlText.Render: string;
var
  i: integer;
begin
  // apply transform
  case Style.Transform of
    ttUppercase: Result := UpperCase(Text);
    ttLowercase: Result := LowerCase(Text);
    ttCapitalize: Result := CapitalizeWords(Text);
    ttSnakeCase: Result := ToSnakeCase(Text);
  else
    Result := Text;  // ttNormalCase and any other case
  end;

  // output text and render any children (children already inherit style via MergeFrom)
  for i := 0 to Children.Count - 1 do
    Result += THtmlNode(Children[i]).Render;
end;

function CreateNodeFromElement(Node: TDOMNode): THtmlNode;
var
  Elem: TDOMElement;
begin
  Result := nil;

  if Node.NodeType <> ELEMENT_NODE then
    Exit;

  Elem := TDOMElement(Node);

  // Known tags → build semantic nodes
  if lowercase(Elem.TagName) = 'body' then
    Result := THtmlBody.Create('')
  else if lowercase(Elem.TagName) = 'div' then
    Result := THtmlDiv.Create(string(Elem.GetAttribute('class')))
  else if lowercase(Elem.TagName) = 'span' then
    Result := THtmlSpan.Create(string(Elem.GetAttribute('class')))
  else if lowercase(Elem.TagName) = 'p' then
    Result := THtmlSpan.Create(string(Elem.GetAttribute('class')))
  else if lowercase(Elem.TagName) = 'a' then
    Result := THtmlA.Create(string(Elem.GetAttribute('class')),
      string(Elem.GetAttribute('href')))
  else if lowercase(Elem.TagName) = 's' then
    Result := THtmlS.Create('')
  else if lowercase(Elem.TagName) = 'b' then
    Result := THtmlB.Create('')
  else if lowercase(Elem.TagName) = 'strong' then
    Result := THtmlB.Create('')
  else if lowercase(Elem.TagName) = 'i' then
    Result := THtmlI.Create('')
  else if lowercase(Elem.TagName) = 'em' then
    Result := THtmlI.Create('')
  else if lowercase(Elem.TagName) = 'ol' then
    Result := THtmlOl.Create((string(Elem.GetAttribute('class'))))
  else if lowercase(Elem.TagName) = 'ul' then
    Result := THtmlUl.Create((string(Elem.GetAttribute('class'))))
  else if lowercase(Elem.TagName) = 'li' then
    Result := THtmlLi.Create((string(Elem.GetAttribute('class'))));

  // Unknown tags → return nil, so TraverseNode will treat them as literal text
end;

procedure TraverseNode(Node: TDOMNode; ParentHtmlNode: THtmlNode);
var
  i:    integer;
  Elem: TDOMElement;
  ChildHtmlNode: THtmlNode;
  TagText: string;
  TextNode: THtmlText;
begin
  Elem := TDOMElement(Node);

  case Node.NodeType of
    ELEMENT_NODE:
    begin
      // Skip adding a <body> element — we already have THtmlBody as root
      if lowercase(Elem.TagName) = 'body' then
      begin
        for i := 0 to Node.ChildNodes.Count - 1 do
          TraverseNode(Node.ChildNodes[i], ParentHtmlNode);
        Exit;
      end;

      ChildHtmlNode := CreateNodeFromElement(Node);

      // Unknown tag or self-closing/fake tag → treat as literal text
      if (ChildHtmlNode = nil) or ((Node.ChildNodes.Count = 0) and
        (not Assigned(Node.Attributes))) then
      begin
        if ChildHtmlNode <> nil then
          ChildHtmlNode.Free; // prevent leak

        TagText := '<' + string(Elem.TagName);

        if Elem.HasAttribute('class') then
          TagText += ' class="' + string(Elem.GetAttribute('class')) + '"';

        TagText += '>';

        TextNode := THtmlText.Create('', TagText);
        TextNode.Style.MergeFrom(ParentHtmlNode.Style);
        ParentHtmlNode.AddChild(TextNode);

        // Only add closing tag if children exist
        if Node.ChildNodes.Count > 0 then
        begin
          for i := 0 to Node.ChildNodes.Count - 1 do
            TraverseNode(Node.ChildNodes[i], ParentHtmlNode);

          TextNode := THtmlText.Create('', '</' + string(Elem.TagName) + '>');
          TextNode.Style.MergeFrom(ParentHtmlNode.Style);
          ParentHtmlNode.AddChild(TextNode);
        end;
      end
      else
      begin
        // Known tag or real element with children
        ChildHtmlNode.Style.MergeFrom(ParentHtmlNode.Style);
        ParentHtmlNode.AddChild(ChildHtmlNode);
        for i := 0 to Node.ChildNodes.Count - 1 do
          TraverseNode(Node.ChildNodes[i], ChildHtmlNode);
      end;
    end;

    TEXT_NODE:
      if Trim(Node.NodeValue) <> '' then
      begin
        TextNode := THtmlText.Create('', string(Node.NodeValue));
        TextNode.Style.MergeFrom(ParentHtmlNode.Style);
        ParentHtmlNode.AddChild(TextNode);
      end;

    DOCUMENT_NODE, DOCUMENT_FRAGMENT_NODE:
      for i := 0 to Node.ChildNodes.Count - 1 do
        TraverseNode(Node.ChildNodes[i], ParentHtmlNode);
  end;
end;

end.

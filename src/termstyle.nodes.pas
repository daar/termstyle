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
  Entities: array[0..0] of record
      name: string;
      char: string;
      end
  = (
    (name: '&;'; char: '&')
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

procedure THtmlNode.ParseBoxClass(const AClass: string);
var
  vstr: string;
  v:    integer;

  function TryParseValue(const S: string; out OutVal: integer): boolean;
  begin
    Result := TryStrToInt(S, OutVal);
    // If you want to support other tokens (e.g. 'px', 'full'), handle them here.
  end;

begin
  // Padding: p-, pt-, pr-, pb-, pl-, px-, py-
  if Copy(AClass, 1, 2) = 'p-' then
  begin
    vstr := Copy(AClass, 3, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Padding.Top := v;
      Style.Padding.Bottom := v;
      Style.Padding.Left := v;
      Style.Padding.Right := v;
    end;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'pt-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Padding.Top := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'pr-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Padding.Right := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'pb-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Padding.Bottom := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'pl-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Padding.Left := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'px-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Padding.Left := v;
      Style.Padding.Right := v;
    end;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'py-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Padding.Top := v;
      Style.Padding.Bottom := v;
    end;
    Exit;
  end;

  // Margin: m-, mt-, mr-, mb-, ml-, mx-, my-
  if Copy(AClass, 1, 2) = 'm-' then
  begin
    vstr := Copy(AClass, 3, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Margin.Top := v;
      Style.Margin.Bottom := v;
      Style.Margin.Left := v;
      Style.Margin.Right := v;
    end;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'mt-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Margin.Top := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'mr-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Margin.Right := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'mb-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Margin.Bottom := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'ml-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then Style.Margin.Left := v;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'mx-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Margin.Left := v;
      Style.Margin.Right := v;
    end;
    Exit;
  end;

  if Copy(AClass, 1, 3) = 'my-' then
  begin
    vstr := Copy(AClass, 4, MaxInt);
    if TryParseValue(vstr, v) then
    begin
      Style.Margin.Top := v;
      Style.Margin.Bottom := v;
    end;
    Exit;
  end;

  // If additional shorthand like 'p-0' or 'm-0' exists, it's already covered above.
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
  innerLength: integer;
  Padded: string;
begin
  // Render children
  Inner := '';
  for i := 0 to Children.Count - 1 do
    Inner += THtmlNode(Children[i]).Render;

  // Calculate length in characters of the inner text (no ANSI)
  innerLength := Length(StripAnsi(Inner));

  // Apply padding (inside the box, using node’s own style)
  Padded := '';

  // Top padding lines
  for i := 1 to Style.Padding.Top do
    Padded +=
      Style.ToAnsi +
      StringOfChar(' ', innerLength + Style.Padding.Left + Style.Padding.Right) +
      RESET_SEQ + 
      sLineBreak;

  // Content lines with left/right padding
  for Line in Inner.Split([sLineBreak]) do
  begin
    PaddedLine :=
      Style.ToAnsi +
      StringOfChar(' ', Style.Padding.Left) +  // left padding
      Line +
      Style.ToAnsi +
      StringOfChar(' ', Style.Padding.Right) + // right padding
      RESET_SEQ;
    Padded += PaddedLine + sLineBreak;
  end;

  // Bottom padding lines
  for i := 1 to Style.Padding.Bottom do
    Padded +=
      Style.ToAnsi +
      StringOfChar(' ', innerLength + Style.Padding.Left + Style.Padding.Right) +
      RESET_SEQ + 
      sLineBreak;

  // Replace Inner with padded version
  Inner := Padded;

  // Apply margin (outside box, using parent’s style)
  Result := '';

  // Top margin
  for i := 1 to Style.Margin.Top do
    Result +=
      Style.Parent.ToAnsi +
      StringOfChar(' ', innerLength + Style.Padding.Left + Style.Padding.Right + Style.Margin.Left + Style.Margin.Right) +
      RESET_SEQ + 
      sLineBreak;

  // Content lines with margins
  for Line in Inner.Split([sLineBreak]) do
  begin
    if Line <> '' then
      Result +=
        Style.Parent.ToAnsi +
        StringOfChar(' ', Style.Margin.Left) +
        Line +
        Style.Parent.ToAnsi +
        StringOfChar(' ', Style.Margin.Right) +
        RESET_SEQ +
        sLineBreak;
  end;

  // Bottom margin
  for i := 1 to Style.Margin.Bottom do
    Result +=
      Style.Parent.ToAnsi +
      StringOfChar(' ', innerLength + Style.Padding.Left + Style.Padding.Right + Style.Margin.Left + Style.Margin.Right) +
      RESET_SEQ +
      sLineBreak;
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
    childRendered +          // rendered children (text + styled content)
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
    ttNormalCase: Result := Text;
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
    Result := THtmlI.Create('');

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

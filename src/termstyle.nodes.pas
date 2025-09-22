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

    function ToAnsi: string;
    procedure AnsiCodeForClass(const AClass: string);
  public
    Style: TTextStyle;

    constructor Create(const AClass: string);
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

function THtmlNode.ToAnsi: string;
var
  AnsiCode: string;
  Attr:     TAnsiAttrEnum;
begin
  AnsiCode := '';

  for Attr in Style.Attrs do
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += AnsiAttrCode[Attr];
  end;

  if Style.FG.Assigned then
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += Format('38;2;%d;%d;%d', [Style.FG.Red, Style.FG.Green,
      Style.FG.Blue]);
  end;

  if Style.BG.Assigned then
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += Format('48;2;%d;%d;%d', [Style.BG.Red, Style.BG.Green,
      Style.BG.Blue]);
  end;

  if AnsiCode = '' then
    Result := ''
  else
    Result := ESC + ANSIcode + 'm';
end;

constructor THtmlNode.Create(const AClass: string);
var
  SingleClass: string;
begin
  inherited Create;

  FClasses := AClass;
  FChildren := TObjectList.Create(True);

  for SingleClass in Self do
    AnsiCodeForClass(lowercase(SingleClass));
end;

destructor THtmlNode.Destroy;
begin
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

function THtmlNode.Render: string;
var
  i: integer;
begin
  // Prepend this node's style (empty if no style)
  Result := Self.ToAnsi;

  // Render each child recursively
  for i := 0 to Children.Count - 1 do
    Result += THtmlNode(Children[i]).Render;

  // Append reset sequence if any style was applied
  if Result <> '' then
    Result += RESET_SEQ;
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
  childRendered: string;
  i: integer;
begin
  // Render all children recursively
  childRendered := '';
  for i := 0 to Children.Count - 1 do
    childRendered += THtmlNode(Children[i]).Render;

  // Wrap the rendered children in the OSC 8 escape sequence for hyperlinks
  Result := ToAnsi +         // ANSI codes for terminal color
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
begin
  // Output the actual text
  Result := Text;

  // Then render children (if any)
  Result += inherited Render;
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
    Result := THtmlDiv.Create(Elem.GetAttribute('class'))
  else if lowercase(Elem.TagName) = 'span' then
    Result := THtmlSpan.Create(Elem.GetAttribute('class'))
  else if lowercase(Elem.TagName) = 'p' then
    Result := THtmlSpan.Create(Elem.GetAttribute('class'))
  else if lowercase(Elem.TagName) = 'a' then
    Result := THtmlA.Create(Elem.GetAttribute('class'), Elem.GetAttribute('href'))
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

        TagText := '<' + Elem.TagName;
        if Elem.HasAttribute('class') then
          TagText += ' class="' + Elem.GetAttribute('class') + '"';
        TagText += '>';
        ParentHtmlNode.AddChild(THtmlText.Create('', TagText));

        // Only add closing tag if children exist
        if Node.ChildNodes.Count > 0 then
        begin
          for i := 0 to Node.ChildNodes.Count - 1 do
            TraverseNode(Node.ChildNodes[i], ParentHtmlNode);
          ParentHtmlNode.AddChild(THtmlText.Create('', '</' + Elem.TagName + '>'));
        end;
      end
      else
      begin
        // Known tag or real element with children
        ParentHtmlNode.AddChild(ChildHtmlNode);
        for i := 0 to Node.ChildNodes.Count - 1 do
          TraverseNode(Node.ChildNodes[i], ChildHtmlNode);
      end;
    end;

    TEXT_NODE:
      if Trim(Node.NodeValue) <> '' then
        ParentHtmlNode.AddChild(THtmlText.Create('', Node.NodeValue));

    DOCUMENT_NODE, DOCUMENT_FRAGMENT_NODE:
      for i := 0 to Node.ChildNodes.Count - 1 do
        TraverseNode(Node.ChildNodes[i], ParentHtmlNode);
  end;
end;

end.

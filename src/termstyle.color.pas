unit TermStyle.Color;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

const
  ESC = #27;
  RESET_SEQ = ESC + '[0m';

type
  // Text transforms
  TTextTransform = (ttNormalCase, ttUppercase, ttLowercase, ttCapitalize, ttSnakeCase);

  // Spacing
  PBoxSpacing = ^TBoxSpacing;
  TBoxSpacing = record
    Top, Right, Bottom, Left: integer;
  end;

  // List styles
  TListStyle = (lsNone, lsDisc, lsDecimal, lsSquare);

  // Enumerate all ANSI attributes you care about
  TAnsiAttrEnum = (
    taBold,        // 1
    taLight,       // 2
    taItalic,      // 3
    taUnderline,   // 4
    taBlink,       // 5
    taInverse,     // 7
    taHidden,      // 8
    taStrike       // 9
    );

  // Set of attributes
  TAnsiAttr = set of TAnsiAttrEnum;

const
  AnsiAttrCode: array[TAnsiAttrEnum] of string = (
    '1', // taBold
    '2', // taLight
    '3', // taItalic
    '4', // taUnderline
    '5', // taBlink
    '7', // taInverse
    '8', // taHidden
    '9'  // taStrike
    );

type
  { THtmlColor }

  THtmlColor = object
    Assigned: boolean;
    Red, Green, Blue: byte;
    class function RGB(const R, G, B: byte): THtmlColor; static;
    class function FromTailwind(const name: string): THtmlColor; static;

    constructor Create;
  end;

  { TTextStyle }

  // Main text style class
  TTextStyle = class
  public
    Parent: TTextStyle;

    // Text colors
    FG, BG: THtmlColor;

    // Attributes (bold, underline, etc.)
    Attrs: TAnsiAttr;

    // Text transforms
    Transform: TTextTransform;

    // Box model
    Margin, Padding: TBoxSpacing;

    // Lists
    ListStyle: TListStyle;

    // Visibility
    Visible: boolean;

    constructor Create;
    procedure Assign(Source: TTextStyle);
    procedure MergeFrom(ParentStyle: TTextStyle);
    function ToAnsi: string;
  end;

implementation

{ THtmlColor }

class function THtmlColor.RGB(const R, G, B: byte): THtmlColor;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;

  Result.Assigned := True;
end;

constructor THtmlColor.Create;
begin
  Assigned := False;
end;

class function THtmlColor.FromTailwind(const name: string): THtmlColor;
begin
  case lowercase(name) of
    // Standard colors
    'black': Result := THtmlColor.RGB(0, 0, 0);
    'red': Result := THtmlColor.RGB(255, 0, 0);
    'green': Result := THtmlColor.RGB(0, 255, 0);
    'yellow': Result := THtmlColor.RGB(128, 128, 0);
    'blue': Result := THtmlColor.RGB(0, 0, 255);
    'magenta': Result := THtmlColor.RGB(128, 0, 128);
    'cyan': Result := THtmlColor.RGB(0, 128, 128);
    'white': Result := THtmlColor.RGB(255, 255, 255);

    // bright colors
    'bright_black': Result := THtmlColor.RGB(128, 128, 128);
    'bright_red': Result := THtmlColor.RGB(255, 0, 0);
    'bright_green': Result := THtmlColor.RGB(0, 255, 0);
    'bright_yellow': Result := THtmlColor.RGB(255, 255, 0);
    'bright_blue': Result := THtmlColor.RGB(0, 0, 255);
    'bright_magenta': Result := THtmlColor.RGB(255, 0, 255);
    'bright_cyan': Result := THtmlColor.RGB(0, 255, 255);
    'bright_white': Result := THtmlColor.RGB(255, 255, 255);

    // tailwind color styles
    'slate-50': Result := THtmlColor.RGB(248, 250, 252);
    'slate-100': Result := THtmlColor.RGB(241, 245, 249);
    'slate-200': Result := THtmlColor.RGB(226, 232, 240);
    'slate-300': Result := THtmlColor.RGB(203, 213, 225);
    'slate-400': Result := THtmlColor.RGB(148, 163, 184);
    'slate-500': Result := THtmlColor.RGB(100, 116, 139);
    'slate-600': Result := THtmlColor.RGB(71, 85, 105);
    'slate-700': Result := THtmlColor.RGB(51, 65, 85);
    'slate-800': Result := THtmlColor.RGB(30, 41, 59);
    'slate-900': Result := THtmlColor.RGB(15, 23, 42);
    'slate-950': Result := THtmlColor.RGB(2, 6, 23);
    'gray-50': Result := THtmlColor.RGB(249, 250, 251);
    'gray-100': Result := THtmlColor.RGB(243, 244, 246);
    'gray-200': Result := THtmlColor.RGB(229, 231, 235);
    'gray-300': Result := THtmlColor.RGB(209, 213, 219);
    'gray-400': Result := THtmlColor.RGB(156, 163, 175);
    'gray-500': Result := THtmlColor.RGB(107, 114, 128);
    'gray-600': Result := THtmlColor.RGB(75, 85, 99);
    'gray-700': Result := THtmlColor.RGB(55, 65, 81);
    'gray-800': Result := THtmlColor.RGB(31, 41, 55);
    'gray-900': Result := THtmlColor.RGB(17, 24, 39);
    'gray-950': Result := THtmlColor.RGB(3, 7, 18);
    'zinc-50': Result := THtmlColor.RGB(250, 250, 250);
    'zinc-100': Result := THtmlColor.RGB(244, 244, 245);
    'zinc-200': Result := THtmlColor.RGB(228, 228, 231);
    'zinc-300': Result := THtmlColor.RGB(212, 212, 216);
    'zinc-400': Result := THtmlColor.RGB(161, 161, 170);
    'zinc-500': Result := THtmlColor.RGB(113, 113, 122);
    'zinc-600': Result := THtmlColor.RGB(82, 82, 91);
    'zinc-700': Result := THtmlColor.RGB(63, 63, 70);
    'zinc-800': Result := THtmlColor.RGB(39, 39, 42);
    'zinc-900': Result := THtmlColor.RGB(24, 24, 27);
    'zinc-950': Result := THtmlColor.RGB(9, 9, 11);
    'neutral-50': Result := THtmlColor.RGB(250, 250, 250);
    'neutral-100': Result := THtmlColor.RGB(245, 245, 245);
    'neutral-200': Result := THtmlColor.RGB(229, 229, 229);
    'neutral-300': Result := THtmlColor.RGB(212, 212, 212);
    'neutral-400': Result := THtmlColor.RGB(163, 163, 163);
    'neutral-500': Result := THtmlColor.RGB(115, 115, 115);
    'neutral-600': Result := THtmlColor.RGB(82, 82, 82);
    'neutral-700': Result := THtmlColor.RGB(64, 64, 64);
    'neutral-800': Result := THtmlColor.RGB(38, 38, 38);
    'neutral-900': Result := THtmlColor.RGB(23, 23, 23);
    'neutral-950': Result := THtmlColor.RGB(10, 10, 10);
    'stone-50': Result := THtmlColor.RGB(250, 250, 249);
    'stone-100': Result := THtmlColor.RGB(245, 245, 244);
    'stone-200': Result := THtmlColor.RGB(231, 229, 228);
    'stone-300': Result := THtmlColor.RGB(214, 211, 209);
    'stone-400': Result := THtmlColor.RGB(168, 162, 158);
    'stone-500': Result := THtmlColor.RGB(120, 113, 108);
    'stone-600': Result := THtmlColor.RGB(87, 83, 78);
    'stone-700': Result := THtmlColor.RGB(68, 64, 60);
    'stone-800': Result := THtmlColor.RGB(41, 37, 36);
    'stone-900': Result := THtmlColor.RGB(28, 25, 23);
    'stone-950': Result := THtmlColor.RGB(12, 10, 9);
    'red-50': Result := THtmlColor.RGB(254, 242, 242);
    'red-100': Result := THtmlColor.RGB(254, 226, 226);
    'red-200': Result := THtmlColor.RGB(254, 202, 202);
    'red-300': Result := THtmlColor.RGB(252, 165, 165);
    'red-400': Result := THtmlColor.RGB(248, 113, 113);
    'red-500': Result := THtmlColor.RGB(239, 68, 68);
    'red-600': Result := THtmlColor.RGB(220, 38, 38);
    'red-700': Result := THtmlColor.RGB(185, 28, 28);
    'red-800': Result := THtmlColor.RGB(153, 27, 27);
    'red-900': Result := THtmlColor.RGB(127, 29, 29);
    'red-950': Result := THtmlColor.RGB(69, 10, 10);
    'orange-50': Result := THtmlColor.RGB(255, 247, 237);
    'orange-100': Result := THtmlColor.RGB(255, 237, 213);
    'orange-200': Result := THtmlColor.RGB(254, 215, 170);
    'orange-300': Result := THtmlColor.RGB(253, 186, 116);
    'orange-400': Result := THtmlColor.RGB(251, 146, 60);
    'orange-500': Result := THtmlColor.RGB(249, 115, 22);
    'orange-600': Result := THtmlColor.RGB(234, 88, 12);
    'orange-700': Result := THtmlColor.RGB(194, 65, 12);
    'orange-800': Result := THtmlColor.RGB(154, 52, 18);
    'orange-900': Result := THtmlColor.RGB(124, 45, 18);
    'orange-950': Result := THtmlColor.RGB(67, 20, 7);
    'amber-50': Result := THtmlColor.RGB(255, 251, 235);
    'amber-100': Result := THtmlColor.RGB(254, 243, 199);
    'amber-200': Result := THtmlColor.RGB(253, 230, 138);
    'amber-300': Result := THtmlColor.RGB(252, 211, 77);
    'amber-400': Result := THtmlColor.RGB(251, 191, 36);
    'amber-500': Result := THtmlColor.RGB(245, 158, 11);
    'amber-600': Result := THtmlColor.RGB(217, 119, 6);
    'amber-700': Result := THtmlColor.RGB(180, 83, 9);
    'amber-800': Result := THtmlColor.RGB(146, 64, 14);
    'amber-900': Result := THtmlColor.RGB(120, 53, 15);
    'amber-950': Result := THtmlColor.RGB(69, 26, 3);
    'yellow-50': Result := THtmlColor.RGB(254, 252, 232);
    'yellow-100': Result := THtmlColor.RGB(254, 249, 195);
    'yellow-200': Result := THtmlColor.RGB(254, 240, 138);
    'yellow-300': Result := THtmlColor.RGB(253, 224, 71);
    'yellow-400': Result := THtmlColor.RGB(250, 204, 21);
    'yellow-500': Result := THtmlColor.RGB(234, 179, 8);
    'yellow-600': Result := THtmlColor.RGB(202, 138, 4);
    'yellow-700': Result := THtmlColor.RGB(161, 98, 7);
    'yellow-800': Result := THtmlColor.RGB(133, 77, 14);
    'yellow-900': Result := THtmlColor.RGB(113, 63, 18);
    'yellow-950': Result := THtmlColor.RGB(66, 32, 6);
    'lime-50': Result := THtmlColor.RGB(247, 254, 231);
    'lime-100': Result := THtmlColor.RGB(236, 252, 203);
    'lime-200': Result := THtmlColor.RGB(217, 249, 157);
    'lime-300': Result := THtmlColor.RGB(190, 242, 100);
    'lime-400': Result := THtmlColor.RGB(163, 230, 53);
    'lime-500': Result := THtmlColor.RGB(132, 204, 22);
    'lime-600': Result := THtmlColor.RGB(101, 163, 13);
    'lime-700': Result := THtmlColor.RGB(77, 124, 15);
    'lime-800': Result := THtmlColor.RGB(63, 98, 18);
    'lime-900': Result := THtmlColor.RGB(54, 83, 20);
    'lime-950': Result := THtmlColor.RGB(26, 46, 5);
    'green-50': Result := THtmlColor.RGB(240, 253, 244);
    'green-100': Result := THtmlColor.RGB(220, 252, 231);
    'green-200': Result := THtmlColor.RGB(187, 247, 208);
    'green-300': Result := THtmlColor.RGB(134, 239, 172);
    'green-400': Result := THtmlColor.RGB(74, 222, 128);
    'green-500': Result := THtmlColor.RGB(34, 197, 94);
    'green-600': Result := THtmlColor.RGB(22, 163, 74);
    'green-700': Result := THtmlColor.RGB(21, 128, 61);
    'green-800': Result := THtmlColor.RGB(22, 101, 52);
    'green-900': Result := THtmlColor.RGB(20, 83, 45);
    'green-950': Result := THtmlColor.RGB(5, 46, 22);
    'emerald-50': Result := THtmlColor.RGB(236, 253, 245);
    'emerald-100': Result := THtmlColor.RGB(209, 250, 229);
    'emerald-200': Result := THtmlColor.RGB(167, 243, 208);
    'emerald-300': Result := THtmlColor.RGB(110, 231, 183);
    'emerald-400': Result := THtmlColor.RGB(52, 211, 153);
    'emerald-500': Result := THtmlColor.RGB(16, 185, 129);
    'emerald-600': Result := THtmlColor.RGB(5, 150, 105);
    'emerald-700': Result := THtmlColor.RGB(4, 120, 87);
    'emerald-800': Result := THtmlColor.RGB(6, 95, 70);
    'emerald-900': Result := THtmlColor.RGB(6, 78, 59);
    'emerald-950': Result := THtmlColor.RGB(2, 44, 34);
    'teal-50': Result := THtmlColor.RGB(240, 253, 250);
    'teal-100': Result := THtmlColor.RGB(204, 251, 241);
    'teal-200': Result := THtmlColor.RGB(153, 246, 228);
    'teal-300': Result := THtmlColor.RGB(94, 234, 212);
    'teal-400': Result := THtmlColor.RGB(45, 212, 191);
    'teal-500': Result := THtmlColor.RGB(20, 184, 166);
    'teal-600': Result := THtmlColor.RGB(13, 148, 136);
    'teal-700': Result := THtmlColor.RGB(15, 118, 110);
    'teal-800': Result := THtmlColor.RGB(17, 94, 89);
    'teal-900': Result := THtmlColor.RGB(19, 78, 74);
    'teal-950': Result := THtmlColor.RGB(4, 47, 46);
    'cyan-50': Result := THtmlColor.RGB(236, 254, 255);
    'cyan-100': Result := THtmlColor.RGB(207, 250, 254);
    'cyan-200': Result := THtmlColor.RGB(165, 243, 252);
    'cyan-300': Result := THtmlColor.RGB(103, 232, 249);
    'cyan-400': Result := THtmlColor.RGB(34, 211, 238);
    'cyan-500': Result := THtmlColor.RGB(6, 182, 212);
    'cyan-600': Result := THtmlColor.RGB(8, 145, 178);
    'cyan-700': Result := THtmlColor.RGB(14, 116, 144);
    'cyan-800': Result := THtmlColor.RGB(21, 94, 117);
    'cyan-900': Result := THtmlColor.RGB(22, 78, 99);
    'cyan-950': Result := THtmlColor.RGB(8, 51, 68);
    'sky-50': Result := THtmlColor.RGB(240, 249, 255);
    'sky-100': Result := THtmlColor.RGB(224, 242, 254);
    'sky-200': Result := THtmlColor.RGB(186, 230, 253);
    'sky-300': Result := THtmlColor.RGB(125, 211, 252);
    'sky-400': Result := THtmlColor.RGB(56, 189, 248);
    'sky-500': Result := THtmlColor.RGB(14, 165, 233);
    'sky-600': Result := THtmlColor.RGB(2, 132, 199);
    'sky-700': Result := THtmlColor.RGB(3, 105, 161);
    'sky-800': Result := THtmlColor.RGB(7, 89, 133);
    'sky-900': Result := THtmlColor.RGB(12, 74, 110);
    'sky-950': Result := THtmlColor.RGB(8, 47, 73);
    'blue-50': Result := THtmlColor.RGB(239, 246, 255);
    'blue-100': Result := THtmlColor.RGB(219, 234, 254);
    'blue-200': Result := THtmlColor.RGB(191, 219, 254);
    'blue-300': Result := THtmlColor.RGB(147, 197, 253);
    'blue-400': Result := THtmlColor.RGB(96, 165, 250);
    'blue-500': Result := THtmlColor.RGB(59, 130, 246);
    'blue-600': Result := THtmlColor.RGB(37, 99, 235);
    'blue-700': Result := THtmlColor.RGB(29, 78, 216);
    'blue-800': Result := THtmlColor.RGB(30, 64, 175);
    'blue-900': Result := THtmlColor.RGB(30, 58, 138);
    'blue-950': Result := THtmlColor.RGB(23, 37, 84);
    'indigo-50': Result := THtmlColor.RGB(238, 242, 255);
    'indigo-100': Result := THtmlColor.RGB(224, 231, 255);
    'indigo-200': Result := THtmlColor.RGB(199, 210, 254);
    'indigo-300': Result := THtmlColor.RGB(165, 180, 252);
    'indigo-400': Result := THtmlColor.RGB(129, 140, 248);
    'indigo-500': Result := THtmlColor.RGB(99, 102, 241);
    'indigo-600': Result := THtmlColor.RGB(79, 70, 229);
    'indigo-700': Result := THtmlColor.RGB(67, 56, 202);
    'indigo-800': Result := THtmlColor.RGB(55, 48, 163);
    'indigo-900': Result := THtmlColor.RGB(49, 46, 129);
    'indigo-950': Result := THtmlColor.RGB(30, 27, 75);
    'violet-50': Result := THtmlColor.RGB(245, 243, 255);
    'violet-100': Result := THtmlColor.RGB(237, 233, 254);
    'violet-200': Result := THtmlColor.RGB(221, 214, 254);
    'violet-300': Result := THtmlColor.RGB(196, 181, 253);
    'violet-400': Result := THtmlColor.RGB(167, 139, 250);
    'violet-500': Result := THtmlColor.RGB(139, 92, 246);
    'violet-600': Result := THtmlColor.RGB(124, 58, 237);
    'violet-700': Result := THtmlColor.RGB(109, 40, 217);
    'violet-800': Result := THtmlColor.RGB(91, 33, 182);
    'violet-900': Result := THtmlColor.RGB(76, 29, 149);
    'violet-950': Result := THtmlColor.RGB(46, 16, 101);
    'purple-50': Result := THtmlColor.RGB(250, 245, 255);
    'purple-100': Result := THtmlColor.RGB(243, 232, 255);
    'purple-200': Result := THtmlColor.RGB(233, 213, 255);
    'purple-300': Result := THtmlColor.RGB(216, 180, 254);
    'purple-400': Result := THtmlColor.RGB(192, 132, 252);
    'purple-500': Result := THtmlColor.RGB(168, 85, 247);
    'purple-600': Result := THtmlColor.RGB(147, 51, 234);
    'purple-700': Result := THtmlColor.RGB(126, 34, 206);
    'purple-800': Result := THtmlColor.RGB(107, 33, 168);
    'purple-900': Result := THtmlColor.RGB(88, 28, 135);
    'purple-950': Result := THtmlColor.RGB(59, 7, 100);
    'fuchsia-50': Result := THtmlColor.RGB(253, 244, 255);
    'fuchsia-100': Result := THtmlColor.RGB(250, 232, 255);
    'fuchsia-200': Result := THtmlColor.RGB(245, 208, 254);
    'fuchsia-300': Result := THtmlColor.RGB(240, 171, 252);
    'fuchsia-400': Result := THtmlColor.RGB(232, 121, 249);
    'fuchsia-500': Result := THtmlColor.RGB(217, 70, 239);
    'fuchsia-600': Result := THtmlColor.RGB(192, 38, 211);
    'fuchsia-700': Result := THtmlColor.RGB(162, 28, 175);
    'fuchsia-800': Result := THtmlColor.RGB(134, 25, 143);
    'fuchsia-900': Result := THtmlColor.RGB(112, 26, 117);
    'fuchsia-950': Result := THtmlColor.RGB(74, 4, 78);
    'pink-50': Result := THtmlColor.RGB(253, 242, 248);
    'pink-100': Result := THtmlColor.RGB(252, 231, 243);
    'pink-200': Result := THtmlColor.RGB(251, 207, 232);
    'pink-300': Result := THtmlColor.RGB(249, 168, 212);
    'pink-400': Result := THtmlColor.RGB(244, 114, 182);
    'pink-500': Result := THtmlColor.RGB(236, 72, 153);
    'pink-600': Result := THtmlColor.RGB(219, 39, 119);
    'pink-700': Result := THtmlColor.RGB(190, 24, 93);
    'pink-800': Result := THtmlColor.RGB(157, 23, 77);
    'pink-900': Result := THtmlColor.RGB(131, 24, 67);
    'pink-950': Result := THtmlColor.RGB(80, 7, 36);
    'rose-50': Result := THtmlColor.RGB(255, 241, 242);
    'rose-100': Result := THtmlColor.RGB(255, 228, 230);
    'rose-200': Result := THtmlColor.RGB(254, 205, 211);
    'rose-300': Result := THtmlColor.RGB(253, 164, 175);
    'rose-400': Result := THtmlColor.RGB(251, 113, 133);
    'rose-500': Result := THtmlColor.RGB(244, 63, 94);
    'rose-600': Result := THtmlColor.RGB(225, 29, 72);
    'rose-700': Result := THtmlColor.RGB(190, 18, 60);
    'rose-800': Result := THtmlColor.RGB(159, 18, 57);
    'rose-900': Result := THtmlColor.RGB(136, 19, 55);
    'rose-950': Result := THtmlColor.RGB(76, 5, 25);
    else
      raise Exception.Create('Unknown ClassName ' + lowercase(name));
  end;
end;

{ TTextStyle }

constructor TTextStyle.Create;
begin
  inherited Create;

  Attrs := [];

  Transform := ttNormalCase;

  Margin := Default(TBoxSpacing);
  Padding := Default(TBoxSpacing);

  ListStyle := lsNone;

  Visible := True;
end;

procedure TTextStyle.Assign(Source: TTextStyle);
begin
  if not Assigned(Source) then Exit;

  FG := Source.FG;
  BG := Source.BG;
  Attrs := Source.Attrs;

  Transform := Source.Transform;

  Margin := Source.Margin;
  Padding := Source.Padding;

  ListStyle := Source.ListStyle;

  Visible := Source.Visible;
end;

procedure TTextStyle.MergeFrom(ParentStyle: TTextStyle);
begin
  if not Assigned(ParentStyle) then Exit;

  Parent := ParentStyle;

  // Merge only when not already set
  if not FG.Assigned and ParentStyle.FG.Assigned then FG := ParentStyle.FG;
  if not BG.Assigned and ParentStyle.BG.Assigned then BG := ParentStyle.BG;

  Attrs := Attrs + ParentStyle.Attrs;

  if Transform = ttNormalCase then Transform := ParentStyle.Transform;

  if Padding.Top = 0 then Padding.Top := ParentStyle.Padding.Top;
  if Padding.Right = 0 then Padding.Right := ParentStyle.Padding.Right;
  if Padding.Bottom = 0 then Padding.Bottom := ParentStyle.Padding.Bottom;
  if Padding.Left = 0 then Padding.Left := ParentStyle.Padding.Left;

  if ListStyle = lsNone then ListStyle := ParentStyle.ListStyle;

  Visible := ParentStyle.Visible;
end;

function TTextStyle.ToAnsi: string;
var
  AnsiCode: string;
  Attr:     TAnsiAttrEnum;
begin
  AnsiCode := '';

  for Attr in Attrs do
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += AnsiAttrCode[Attr];
  end;

  if FG.Assigned then
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += Format('38;2;%d;%d;%d', [FG.Red, FG.Green, FG.Blue]);
  end;

  if BG.Assigned then
  begin
    if AnsiCode <> '' then
      AnsiCode += ';';
    AnsiCode += Format('48;2;%d;%d;%d', [BG.Red, BG.Green, BG.Blue]);
  end;

  if AnsiCode = '' then
    Result := ''
  else
    Result := ESC + '[' + ANSIcode + 'm';
end;

end.

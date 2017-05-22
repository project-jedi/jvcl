object JvgfCheckVersionInfo: TJvgfCheckVersionInfo
  Left = 466
  Top = 175
  ActiveControl = reComments
  BorderStyle = bsDialog
  Caption = 'Checking for new version'
  ClientHeight = 298
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 258
    Width = 428
    Height = 2
    Anchors = [akLeft, akBottom]
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 74
    Width = 74
    Height = 13
    Caption = 'Current version:'
  end
  object Label2: TLabel
    Left = 16
    Top = 98
    Width = 66
    Height = 13
    Caption = 'Release date:'
  end
  object Label3: TLabel
    Left = 216
    Top = 74
    Width = 60
    Height = 13
    Caption = 'Last version:'
  end
  object Label4: TLabel
    Left = 216
    Top = 98
    Width = 66
    Height = 13
    Caption = 'Release date:'
  end
  object Label5: TLabel
    Left = 16
    Top = 136
    Width = 153
    Height = 13
    Caption = 'Download page for new version:'
  end
  object Shape1: TShape
    Left = 11
    Top = 125
    Width = 402
    Height = 1
    Pen.Color = clBtnShadow
  end
  object lURL: TLabel
    Left = 16
    Top = 153
    Width = 404
    Height = 13
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'http://shop.biblio-globus.ru/cpr/SiteBuilder/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lURLClick
  end
  object Label7: TLabel
    Left = 16
    Top = 180
    Width = 47
    Height = 13
    Caption = 'More info:'
  end
  object Bevel2: TBevel
    Left = 12
    Top = 195
    Width = 405
    Height = 55
  end
  object sbNext: TButton
    Left = 335
    Top = 268
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = sbNextClick
  end
  object eCurentVersion: TEdit
    Left = 107
    Top = 74
    Width = 89
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
  object eCurentVersionDate: TEdit
    Left = 107
    Top = 100
    Width = 89
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object eLastVersion: TEdit
    Left = 323
    Top = 74
    Width = 89
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object eLastVersionDate: TEdit
    Left = 323
    Top = 100
    Width = 89
    Height = 21
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
  end
  object reComments: TRichEdit
    Left = 14
    Top = 198
    Width = 400
    Height = 50
    BorderStyle = bsNone
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 66
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWindow
    TabOrder = 6
    object Label6: TLabel
      Left = 16
      Top = 8
      Width = 295
      Height = 13
      Caption = 'Checking for new version of SiteBuilder application.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 32
      Top = 25
      Width = 295
      Height = 26
      Caption = 
        'Information about updates is downloading from www-server os the ' +
        'developers.'
      WordWrap = True
    end
    object Image1: TImage
      Left = 392
      Top = 16
      Width = 26
      Height = 24
      AutoSize = True
      Picture.Data = {
        07544269746D6170D6060000424DD60600000000000036040000280000001A00
        0000180000000100080000000000A00200000000000000000000000100000000
        0000000000000000800000800000008080008000000080008000808000008080
        80001C1414008484840074544C00A4745C00D4948400746C6C00BCBCBC005C44
        3C00845C5400B4B4B400C48C7C00ECA48C003C242400949494007C5C5400B47C
        6C00DC9C840084746C00E4E4E400644C44008C645C00ACA4EC00000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF001A1A1A1A1A1A1A1A1A1A1A1A1A1A1A0E111A1A1A1A1A1A1A1A1A00001A1A
        1A1A1A1A1A1A1A1A1A1A1AF80914080E1A1A1A1A1A1A1A1A00001A1A1A1A1A1A
        1A1A1A1A1AF80914141B081B0E1A1A1A1A1A1A1A00001A1A1A1A1AFF1A1A1A0E
        1914140F1B1B140F091A1A1AFF1A1A1A00001A1AFF1A1A1A1A0E0D14140F1B1B
        1B0F1416140E1A1A1A1A1A1A00001A1A1A1A1A0E0D08140F1B1B1B0F1B0A1416
        0A1BF81A1A1A1A1A00001A1A1A1A0908140F0F1B1B1B0F1B0F0A140A1C0F0D1A
        1A1A1A1A00001A1AF8110F0F0F0F0F1B0F1B0F1B1B0A141B1C1614151AFF1A1A
        00001AF8F815141B0F0F0F1B0F1B1B1B0F0A0F0F1C160A0FF81A1A1A00001AF8
        0E09140F0F0F1B0F1B0F1B0F1B0A0F0F1C16160F151A1A1A00001A1115090F0F
        0F0F0F1B1B1B1B1B1B0A0F0F1010160F091A1A1A00001A15090D0F0F0F1B1B1B
        0F1B0F0F0A0A0F0F0B160A0A191A1A1A00001A0E150D0F0F1B1B1B0F1B1B1B0A
        0A160F140B0A16160FF81A1A00001A1AF80D0F0F1B1B0F1B0F0A160A0F141414
        101C16160FF81A1A00001A1A1A0D0F1B1B0F1B0A0A0F14140F0B180C081C1016
        0F111A1A00001A1AF80F0F1B0A0F0F14140F190C13130C0C0C0816100F151A1A
        00001A1AF8140A0F14080F190C1313180C0C0C0C1812081C0A091A1A00001A1A
        0E0814141C0C130C0C0C120C180C180C181317081C191A1A00001A1A0E0F1713
        0C0C1212121212120C121212120C1317081BF81A00001A1A1A1A151C12121212
        0C1212121212120C121212131708F81A00001A1A1A1A1AF8190B0C1212121212
        12121212181217170C0F111A00001A1A1A1A1A1AF811190B121212120C121717
        1C1915110E1A1A1A00001A1A1A1A1A1A1A1AF8151617170B1C19150EF8F81A1A
        1A1A1A1A00001A1A1AFF1A1A1A1A1A1AF8091511F8F81A1A1A1A1A1A1A1A1A1A
        0000}
      Transparent = True
    end
  end
end

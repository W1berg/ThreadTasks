object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 1500
  ClientWidth = 1200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 1200
    Height = 1500
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Memo1: TMemo
      Left = 0
      Top = 41
      Width = 449
      Height = 1459
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object pnlTop: TFlowPanel
      Left = 0
      Top = 0
      Width = 1200
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object btnStart: TButton
        Left = 0
        Top = 0
        Width = 75
        Height = 28
        Caption = 'btnStart'
        TabOrder = 0
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 75
        Top = 0
        Width = 75
        Height = 28
        Caption = 'btnStop'
        TabOrder = 1
        OnClick = btnStopClick
      end
      object btnUpdate: TButton
        Left = 150
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Update'
        TabOrder = 2
        OnClick = btnUpdateClick
      end
      object btnDelete: TButton
        Left = 200
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pnlThreads: TScrollBox
      Left = 449
      Top = 41
      Width = 751
      Height = 1459
      Align = alClient
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 568
    Top = 488
  end
end

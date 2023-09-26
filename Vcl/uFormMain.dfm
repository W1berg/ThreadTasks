object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 1000
  ClientWidth = 3000
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
    Width = 3000
    Height = 1000
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 800
    object Splitter1: TSplitter
      Left = 0
      Top = 441
      Width = 3000
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = -48
      ExplicitTop = 435
    end
    object Memo1: TMemo
      Left = 0
      Top = 41
      Width = 3000
      Height = 400
      Align = alTop
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
      Width = 3000
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object btnStart: TButton
        Left = 0
        Top = 0
        Width = 50
        Height = 28
        Caption = 'Start'
        TabOrder = 0
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 50
        Top = 0
        Width = 50
        Height = 28
        Caption = 'Stop'
        TabOrder = 1
        OnClick = btnStopClick
      end
      object btnRefresh: TButton
        Left = 100
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Refresh'
        TabOrder = 2
        OnClick = btnRefreshClick
      end
      object btnDelete: TButton
        Left = 150
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
    end
    object pnlTasks: TFlowPanel
      Left = 0
      Top = 444
      Width = 3000
      Height = 556
      Align = alClient
      TabOrder = 2
      ExplicitTop = 657
      ExplicitHeight = 343
    end
  end
  object tiRefresh: TTimer
    Interval = 0
    OnTimer = tiRefreshTimer
    Left = 568
    Top = 488
  end
end

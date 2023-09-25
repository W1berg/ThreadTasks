// Copyright (C) 2023 Pontus Wiberg <wiberg.public@outlook.com>
// Licensed under GPLv2 or later version. Contact for any licensing concerns

unit uFrmThread;

interface

uses
  uTypes,

  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons;

type
  TFrameThread = class(TFrame)

    Panel1: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    memLog: TMemo;
    btnDelete: TButton;
    ediStatus: TEdit;
    pnlTop: TPanel;
    sbtUpdate: TSpeedButton;

    procedure btnUpdateClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure sbtUpdateClick(Sender: TObject);
  private
    FDesignHeight: Integer;
    FThread: ITaskAbstract;

  public
    FLog: TStringList;

    constructor Create(const AOwner: TComponent; AThread: ITaskAbstract); reintroduce;
    destructor Destroy; override;

    function DeleteEnabled: Boolean;
    procedure Log(const AStr: string);
    procedure Updatee;
  end;

implementation

{$R *.dfm}

procedure TFrameThread.btnStartClick(Sender: TObject);
begin
  FThread.Run;
  Updatee;
end;

procedure TFrameThread.btnStopClick(Sender: TObject);
begin
  FThread.Terminate;
  Updatee;
end;

procedure TFrameThread.btnUpdateClick(Sender: TObject);
begin
  Updatee;
end;

procedure TFrameThread.btnDeleteClick(Sender: TObject);
begin
  Destroy;
end;

constructor TFrameThread.Create(const AOwner: TComponent; AThread: ITaskAbstract);
begin
  inherited Create(AOwner);

  FDesignHeight := Self.Height;

  btnDelete.Enabled := False;

  FLog := TStringList.Create;
  FThread := AThread;

  Updatee;
end;

destructor TFrameThread.Destroy;
begin
  FLog.Free;
  inherited;
end;

function TFrameThread.DeleteEnabled: Boolean;
begin
  Result := btnDelete.Enabled;
end;

procedure TFrameThread.Log(const AStr: string);
var
  LStr: string;
begin
  if Assigned(FThread) then
  begin
    LStr := FThread.GetName;
    memLog.Lines.Add(AStr);
  end
  else
    FGet.Fac.Log.Log(AStr);
end;

procedure TFrameThread.sbtUpdateClick(Sender: TObject);
begin
  Updatee;
end;

procedure TFrameThread.Updatee;
var
  L: Integer;
begin
  // Exit;

  if not Assigned(FThread) then
  begin
    ediStatus.Text := 'Not assigned';
    Exit;
  end;

  ediStatus.Text := FThread.GetName + ': ';

  if FThread.IsDone then
  begin
    ediStatus.Text := ediStatus.Text + 'Done';
    btnDelete.Enabled := True;
    sbtUpdate.Down := False;

    btnStart.Enabled := False;
    btnStop.Enabled := False;
  end
  else
  begin
    if FThread.IsTerminated then
    begin
      ediStatus.Text := ediStatus.Text + 'Terminated';
      btnStart.Enabled := False;
      btnStop.Enabled := False;
    end
    else if FThread.IsStarted then
    begin
      ediStatus.Text := ediStatus.Text + 'Started';
      btnStart.Enabled := False;
      btnStop.Enabled := True;
    end;
  end;

  // Exit;

  if memLog.Lines.Count = 0 then
  begin
    Self.Height := pnlTop.Height;
  end
  else
  begin
    L := pnlTop.Height + Round(memLog.Font.Size * 2 + memLog.Font.Size * memLog.Lines.Count * 1.8);

    if L > FDesignHeight then
      L := FDesignHeight;

    Self.Height := L;

    // memLog.Lines.Text := FLog.Text;
    // SendMessage(memLog.Handle, EM_LINESCROLL, 0, memLog.Lines.Count - 1);
  end;
end;

end.

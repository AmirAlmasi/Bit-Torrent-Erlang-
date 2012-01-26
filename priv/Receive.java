/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import com.ericsson.otp.erlang.*;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.net.*;
import gui.ErBit;
import java.text.NumberFormat;
import java.util.ArrayList;
import javax.swing.table.DefaultTableModel;

/**
 * 
 * @author Deno
 */
public class Receive {
	private int id;
	private OtpNode node;
	private OtpMbox mbox;
	private OtpErlangObject receiveObject;
	private OtpErlangTuple msg;
	private OtpErlangTuple tempTuple;
	private OtpErlangString torrentName;
	private OtpErlangList trackers;
	private OtpErlangString files;
	private OtpErlangLong identifier;
	private OtpErlangLong seeders;
	private OtpErlangLong leechers;
	private OtpErlangLong portInt;
	private OtpErlangLong downSpeed;
	private OtpErlangLong upSpeed;
	private OtpErlangDouble status;
	private OtpErlangLong size;
	private OtpErlangLong downloaded;
	private OtpErlangLong uploaded;
	private OtpErlangList ips;
	private OtpErlangList tempList;
	private OtpErlangAtom command;
	private OtpErlangBinary tempBinary;
	Identifier control = new Identifier();

	public Receive() {
		try {
			node = new OtpNode("java", "cookie");
			mbox = node.createMbox("gui");
		} catch (IOException ex) {
			Logger.getLogger(ErBit.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	public void receiveMessages() throws ArrayIndexOutOfBoundsException {
		try {
			receiveObject = mbox.receive();
			msg = (OtpErlangTuple) receiveObject;
			command = (OtpErlangAtom) msg.elementAt(0);
			identifier = (OtpErlangLong) msg.elementAt(1);

			try {
				int i = identifier.intValue();
				int a = control.findRowId(i);

				if (a == -1) {
					control.setId(i);
					// int b = addRowA(ErBit.model1);
				}

			} catch (OtpErlangRangeException ex) {
				Logger.getLogger(Receive.class.getName()).log(Level.SEVERE,
						null, ex);
			}

			if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("name").toString())) {
				receivedNewTorrent();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("ips").toString())) {
				receivedIp();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("downSpeed").toString())) {
				receivedDownSpeed();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("upSpeed").toString())) {
				receivedUpSpeed();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("status").toString())) {
				receivedStatus();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("size").toString())) {
				receivedSize();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("seeders").toString())) {
				receivedSeeders();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("leechers").toString())) {
				receivedLeechers();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("downloaded").toString())) {
				receivedDownloaded();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("uploaded").toString())) {
				receivedUploaded();
				updateTree();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("trackers").toString())) {
				receivedTrackers();
			} else if (command.toString().equalsIgnoreCase(
					new OtpErlangAtom("files").toString())) {
				receivedFiles();
			}

		} catch (OtpErlangExit ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		} catch (OtpErlangDecodeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		} catch (ArrayIndexOutOfBoundsException ex) {
			System.out.println("Error: " + ex.getMessage());
			System.out.println("ArrayError: " + ex.getStackTrace());
		}

	}

	public void updateTree() {
		long time = control.getTableTime();
		if (time > 1000) {
			control.setTableTime();
			int row[] = ErBit.jTable1.getSelectedRows();
			if (ErBit.jTree1.getSelectionPath().toString()
					.compareTo("[Torrents]") == 0) {

				removeList(ErBit.model1);
				updateTorrents(0);

			} else if (ErBit.jTree1.getSelectionPath().toString()
					.compareTo("[Torrents, Downloading]") == 0) {

				removeList(ErBit.model1);
				updateTorrents(1);
			} else if (ErBit.jTree1.getSelectionPath().toString()
					.compareTo("[Torrents, Uploading]") == 0) {

				removeList(ErBit.model1);
				updateTorrents(2);
			} else if (ErBit.jTree1.getSelectionPath().toString()
					.compareTo("[Torrents, Finished]") == 0) {

				removeList(ErBit.model1);
				updateTorrents(3);
			}/*
			 * else{ removeList(ErBit.model1); updateTorrents(0); }
			 */
			for (int a = 0; a < row.length; a++) {
				ErBit.jTable1.addRowSelectionInterval(row[a], row[a]);
				updateList(row[a]);
			}
		}
	}

	private void receivedNewTorrent() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		torrentName = (OtpErlangString) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Filename");
			// int n = ErBit.jTable1.getColumnModel().getColumnIndex("Nr");
			int t = ErBit.model1.findColumn("Filename");
			control.setTorrents(i, t, torrentName.toString());
			// ErBit.jTable1.setValueAt(torrentName.toString(), i, m);
			// ErBit.jTable1.setValueAt(i +1 , i, n);
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedIp() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		System.out.println(msg.elementAt(2).toString());
		ips = (OtpErlangList) msg.elementAt(2);
		try {
			int a = control.findRowId(identifier.intValue());
			control.info.get(a).clearPeers();

			for (int i = 0; i < ips.elements().length; i++) {
				tempList = (OtpErlangList) ips.getNthTail(i);
				System.out.println(tempList.toString());
				for (int j = 0; j < 2; j++) {
					System.out.println(j);
					if (j == 0) {
						String ip = "";
						tempList = (OtpErlangList) tempList.getHead();
						System.out.println(tempList.toString());
						tempTuple = (OtpErlangTuple) tempList.getHead();
						int m = 0;
						for (m = 0; m < tempTuple.elements().length - 1; m++) {
							ip = ip + tempTuple.elementAt(m).toString() + ".";
						}
						ip = ip + tempTuple.elementAt(m).toString();
						control.setPeers(a, ip);
					} else {
						tempList = (OtpErlangList) tempList.getTail();
						portInt = (OtpErlangLong) tempList.getHead();
						control.setPeers(a, portInt.toString());
					}
				}
			}
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}
	}

	private void receivedDownSpeed() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		downSpeed = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Download Speed");
			// ErBit.jTable1.setValueAt(downSpeed.toString() + " ?/s", i, m);
			int t = ErBit.model1.findColumn("Download Speed");
			control.setTorrents(i, t, setSpeedFormat(downSpeed.toString()));
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedUpSpeed() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		upSpeed = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Upload Speed");
			// ErBit.jTable1.setValueAt(upSpeed.toString() + " ?/s", i, m);
			int t = ErBit.model1.findColumn("Upload Speed");
			control.setTorrents(i, t, setSpeedFormat(upSpeed.toString()));
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedStatus() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		status = (OtpErlangDouble) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m = ErBit.jTable1.getColumnModel().getColumnIndex("Status");
			// ErBit.jTable1.setValueAt(status.toString() + " %", i, m);
			int t = ErBit.model1.findColumn("Status");
			control.setTorrents(i, t, status.toString() + " %");
			if (status.doubleValue() == 100.00) {
				control.setType(i, 2);
			} else {
				control.setType(i, 1);
			}
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedSize() {
		identifier = (OtpErlangLong) msg.elementAt(1);
		size = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m = ErBit.jTable1.getColumnModel().getColumnIndex("Size");
			// ErBit.jTable1.setValueAt(size.toString() + " ?b", i, m);
			int t = ErBit.model1.findColumn("Size");
			control.setTorrents(i, t, size.toString());
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedSeeders() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		seeders = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m = ErBit.jTable1.getColumnModel().getColumnIndex("Seeders");
			// ErBit.jTable1.setValueAt(seeders.toString(), i, m);
			int t = ErBit.model1.findColumn("Seeders");
			control.setTorrents(i, t, seeders.toString());
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedLeechers() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		leechers = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Leechers");
			// ErBit.jTable1.setValueAt(leechers.toString(), i, m);
			int t = ErBit.model1.findColumn("Leechers");
			control.setTorrents(i, t, leechers.toString());
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedDownloaded() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		downloaded = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Downloaded");
			// ErBit.jTable1.setValueAt(downloaded.toString() + " ?b", i, m);
			int t = ErBit.model1.findColumn("Downloaded");
			control.setTorrents(i, t, downloaded.toString());
			long time = control.getDownTime(i) / 1000;
			// System.out.println("TIME: " + time);
			control.setDownData(i, downloaded.longValue());
			if (time >= 1) {
				long total = control.getDownData(i);
				control.setDownTime(i);
				t = ErBit.model1.findColumn("Download Speed");
				control.setTorrents(i, t, setSpeedFormat(Long.toString(total)));
			}

		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedUploaded() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		uploaded = (OtpErlangLong) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			// int m =
			// ErBit.jTable1.getColumnModel().getColumnIndex("Uploaded");
			// ErBit.jTable1.setValueAt(uploaded.toString() + " ?b", i, m);
			int t = ErBit.model1.findColumn("Uploaded");
			control.setTorrents(i, t, uploaded.toString());

			long time = control.getUpTime(i) / 1000;
			// System.out.println("TIME: " + time);
			control.setUpData(i, uploaded.longValue());
			if (time >= 1) {
				long total = control.getUpData(i);
				control.setUpTime(i);
				t = ErBit.model1.findColumn("Upload Speed");
				control.setTorrents(i, t, setSpeedFormat(Long.toString(total)));
			}

		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedTrackers() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		trackers = (OtpErlangList) msg.elementAt(2);
		try {
			int a = control.findRowId(identifier.intValue());
			control.info.get(a).clearTrackers();
			for (int i = 0; i < trackers.elements().length; i++) {
				tempBinary = (OtpErlangBinary) trackers.elementAt(i);
				control.setTrackers(a, tempBinary.binaryValue().toString());
			}

		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}

	}

	private void receivedFiles() {

		identifier = (OtpErlangLong) msg.elementAt(1);
		files = (OtpErlangString) msg.elementAt(2);
		try {
			int i = control.findRowId(identifier.intValue());
			control.setFiles(i, files.toString());
		} catch (OtpErlangRangeException ex) {
			Logger.getLogger(Receive.class.getName()).log(Level.SEVERE, null,
					ex);
		}
	}

	public String setSizeFormat(String i) {

		long checkSize = Long.parseLong(i);
		long checkGb = 1073741824L;
		long checkMb = 1048576;
		long checkKb = 1024L;
		double sizeS = Double.parseDouble(i);
		double gb = 1073741824;
		double mb = 1048576;
		double kb = 1024;
		NumberFormat r = NumberFormat.getInstance();
		r.setMinimumFractionDigits(2);
		r.setMaximumFractionDigits(2);

		if (checkSize / checkGb > 0) {
			return r.format(sizeS / gb) + " GB";
		} else if (checkSize / checkMb > 0) {
			return r.format(sizeS / mb) + " MB";
		} else if (checkSize / checkKb > 0) {
			return r.format(sizeS / kb) + " KB";
		}

		return Double.toString(sizeS) + " B";
	}

	private String setSpeedFormat(String i) {

		long checkSize = Long.parseLong(i);
		long checkGb = 1073741824L;
		long checkMb = 1048576;
		long checkKb = 1024L;
		double sizeS = Double.parseDouble(i);
		double gb = 1073741824;
		double mb = 1048576;
		double kb = 1024;
		NumberFormat r = NumberFormat.getInstance();
		r.setMinimumFractionDigits(2);
		r.setMaximumFractionDigits(2);

		if (checkSize / checkGb > 0) {
			return r.format(sizeS / gb) + " GB/s";
		} else if (checkSize / checkMb > 0) {
			return r.format(sizeS / mb) + " MB/s";
		} else if (checkSize / checkKb > 0) {
			return r.format(sizeS / kb) + " KB/s";
		}

		return Double.toString(sizeS) + " B/s";
	}

	public void sendPath(String completeName, String directoryName,
			int numWant, int globalNumWant, int port) {
		try {
			String computername = InetAddress.getLocalHost().getHostName();
			OtpErlangObject[] tuple = new OtpErlangObject[7];
			tuple[0] = new OtpErlangAtom("init");
			tuple[1] = new OtpErlangString(completeName);
			tuple[2] = new OtpErlangString(directoryName);
			tuple[3] = new OtpErlangInt(numWant);
			tuple[4] = new OtpErlangInt(globalNumWant);
			tuple[5] = new OtpErlangInt(port);
			tuple[6] = mbox.self();
			mbox.send("erbit_guiConnector", "connect@" + computername,
					new OtpErlangTuple(tuple));
			System.out.println(new OtpErlangTuple(tuple));
			// receiveMessages();
		} catch (Exception e) {
			System.out.println("Exception caught =" + e.getMessage());
		}
	}

	public void sendStart(int id) {
		try {
			String computername = InetAddress.getLocalHost().getHostName();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangAtom("start");
			tuple[1] = new OtpErlangInt(id);
			mbox.send("erbit_guiConnector", "connect@" + computername,
					new OtpErlangTuple(tuple));
			System.out.println(new OtpErlangTuple(tuple));
			// receiveMessages();
		} catch (Exception e) {
			System.out.println("Exception caught =" + e.getMessage());
		}
	}

	public void sendPause(int id) {
		try {
			String computername = InetAddress.getLocalHost().getHostName();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangAtom("pause");
			tuple[1] = new OtpErlangInt(id);
			mbox.send("erbit_guiConnector", "connect@" + computername,
					new OtpErlangTuple(tuple));
			System.out.println(new OtpErlangTuple(tuple));
			// receiveMessages();
		} catch (Exception e) {
			System.out.println("Exception caught =" + e.getMessage());
		}
	}

	public void sendStop(int id) {
		try {
			String computername = InetAddress.getLocalHost().getHostName();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangAtom("stop");
			tuple[1] = new OtpErlangInt(id);
			mbox.send("erbit_guiConnector", "connect@" + computername,
					new OtpErlangTuple(tuple));
			System.out.println(new OtpErlangTuple(tuple));
			// receiveMessages();
		} catch (Exception e) {
			System.out.println("Exception caught =" + e.getMessage());
		}
	}

	public void sendRemove(int id) {
		try {
			String computername = InetAddress.getLocalHost().getHostName();
			OtpErlangObject[] tuple = new OtpErlangObject[2];
			tuple[0] = new OtpErlangAtom("remove");
			tuple[1] = new OtpErlangInt(id);
			mbox.send("erbit_guiConnector", "connect@" + computername,
					new OtpErlangTuple(tuple));
			System.out.println(new OtpErlangTuple(tuple));
			// receiveMessages();
		} catch (Exception e) {
			System.out.println("Exception caught =" + e.getMessage());
		}
	}

	public void addRowA(DefaultTableModel model) {
		Object rows[] = { " ", " ", " ", " ", " ", " ", " ", " ", " ", " " };
		model.addRow(rows);
		// return model.getRowCount() - 1;
	}

	public void removeRow(DefaultTableModel model, int rowNumber) {
		model.removeRow(rowNumber);
		control.removeId(rowNumber);
		for (int i = 0; i < ErBit.jTable1.getRowCount(); i++) {
			ErBit.jTable1.setValueAt(i + 1, i, 0);
		}
	}

	public void removeList(DefaultTableModel model) {
		int rowcount = model.getRowCount();
		for (int i = 0; i < rowcount; i++) {
			model.removeRow(0);
		}
	}

	public void updateList(int i) {
		ArrayList<String> a;
		a = control.getPeers(i);
		removeList(ErBit.model2);
		for (int j = 0, m = 0; j < a.size(); j = j + 2, m++) {
			addRowA(ErBit.model2);
			ErBit.jTable2.setValueAt(a.get(j), m, 0);
			ErBit.jTable2.setValueAt(a.get(j + 1), m, 1);
		}
		a = control.getTrackers(i);
		removeList(ErBit.model3);
		for (int j = 0; j < a.size(); j++) {
			addRowA(ErBit.model3);
			ErBit.jTable3.setValueAt(a.get(j), j, 0);
		}

		a = control.getFiles(i);
		removeList(ErBit.model4);
		for (int j = 0; j < a.size(); j++) {
			addRowA(ErBit.model4);
			ErBit.jTable4.setValueAt(a.get(j), j, 0);
		}

	}

	public void updateTorrents(int i) {
		int counter = 0;
		for (int j = 0; j < control.torrents.size(); j++) {
			if (control.getType(j) == i || i == 0) {
				addRowA(ErBit.model1);
				ErBit.jTable1.setValueAt(counter + 1, counter, 0);
				for (int n = 1; n < 10; n++) {
					ErBit.jTable1.setValueAt(control.getTorrents(j, n),
							counter, n);
				}
				counter++;
			}
		}
	}

}

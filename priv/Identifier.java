/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.util.ArrayList;
import java.util.Calendar;

/**
 * 
 * @author Deno
 */
public class Identifier {
	ArrayList<Integer> array = new ArrayList();
	ArrayList<PeerList> info = new ArrayList();
	ArrayList<Torrents> torrents = new ArrayList();
	ArrayList<Timer> downTimer = new ArrayList();
	ArrayList<Timer> upTimer = new ArrayList();
	long tableTime;
	long updateTime;

	public void setId(int i) {
		array.add(i);
		info.add(new PeerList(i));
		torrents.add(new Torrents(i));
		downTimer.add(new Timer());
		upTimer.add(new Timer());
		Calendar cal = Calendar.getInstance();
		tableTime = cal.getTimeInMillis();
		// System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
	}

	public void removeId(int i) {
		array.remove(i);
		info.remove(i);
		torrents.remove(i);
		downTimer.remove(i);
		upTimer.remove(i);

	}

	public int getId(int i) {
		return array.get(i);
	}

	public int findRowId(int i) {
		return array.indexOf(i);
	}

	public void setPeers(int i, String s) {
		info.get(i).setPeers(s);
	}

	public void setTrackers(int i, String s) {
		info.get(i).setTrackers(s);
	}

	public void setFiles(int i, String s) {
		info.get(i).setFiles(s);
	}

	public void setType(int row, int i) {
		torrents.get(row).setType(i);
	}

	public void setDownTime(int row) {
		downTimer.get(row).setTime();
	}

	public void setUpTime(int row) {
		upTimer.get(row).setTime();
	}

	public void setDownData(int row, long size) {
		downTimer.get(row).addData(size);
	}

	public void setUpData(int row, long size) {
		upTimer.get(row).addData(size);
	}

	public long getDownData(int row) {
		return downTimer.get(row).getData();
	}

	public long getUpData(int row) {
		return upTimer.get(row).getData();
	}

	public long getDownTime(int row) {
		return downTimer.get(row).getTime();
	}

	public long getUpTime(int row) {
		return upTimer.get(row).getTime();
	}

	public int getType(int row) {
		return torrents.get(row).getType();
	}

	public void setTorrents(int row, int column, String s) {
		if (column == 1) {
			torrents.get(row).setFileName(s);
		} else if (column == 2) {
			torrents.get(row).setSize(s);
		} else if (column == 3) {
			torrents.get(row).setStatus(s);
		} else if (column == 4) {
			torrents.get(row).setDspeed(s);
		} else if (column == 5) {
			torrents.get(row).setUpspeed(s);
		} else if (column == 6) {
			torrents.get(row).setSeeders(s);
		} else if (column == 7) {
			torrents.get(row).setLeechers(s);
		} else if (column == 8) {
			torrents.get(row).setDownloaded(s);
		} else if (column == 9) {
			torrents.get(row).setUploaded(s);
		}
	}

	public String getTorrents(int row, int column) {
		if (column == 1) {
			return torrents.get(row).getFileName();
		} else if (column == 2) {
			return torrents.get(row).getSize();
		} else if (column == 3) {
			return torrents.get(row).getStatus();
		} else if (column == 4) {
			return torrents.get(row).getDspeed();
		} else if (column == 5) {
			return torrents.get(row).getUpspeed();
		} else if (column == 6) {
			return torrents.get(row).getSeeders();
		} else if (column == 7) {
			return torrents.get(row).getLeechers();
		} else if (column == 8) {
			return torrents.get(row).getDownloaded();
		} else if (column == 9) {
			return torrents.get(row).getUploaded();
		} else
			return null;
	}

	public ArrayList getPeers(int i) {
		return info.get(i).getPeers();
	}

	public ArrayList getTrackers(int i) {
		return info.get(i).getTrackers();
	}

	public ArrayList getFiles(int i) {
		return info.get(i).getFiles();
	}

	public void setTableTime() {
		Calendar cal = Calendar.getInstance();
		tableTime = cal.getTimeInMillis();
	}

	public long getTableTime() {
		Calendar cal = Calendar.getInstance();
		updateTime = cal.getTimeInMillis();
		return updateTime - tableTime;
	}
}

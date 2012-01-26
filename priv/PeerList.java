/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.util.ArrayList;

/**
 * 
 * @author Deno
 */
public class PeerList {
	ArrayList<String> peers;
	ArrayList<String> trackers;
	ArrayList<String> files;
	int id;

	public PeerList(int i) {
		id = i;
		peers = new ArrayList();
		trackers = new ArrayList();
		files = new ArrayList();
	}

	public void setPeers(String a) {
		peers.add(a);
	}

	public void setTrackers(String a) {
		trackers.add(a);
	}

	public void setFiles(String a) {
		files.add(a);
	}

	public ArrayList getPeers() {
		return peers;
	}

	public ArrayList getTrackers() {
		return trackers;
	}

	public ArrayList getFiles() {
		return files;
	}

	public int getId() {
		return id;
	}

	public void clearPeers() {
		peers.clear();
	}

	public void clearInfo() {
		trackers.clear();
		files.clear();
	}
}

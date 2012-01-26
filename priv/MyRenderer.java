/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gui;

import java.awt.Component;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

class MyRenderer extends DefaultTreeCellRenderer {

	@Override
	public Component getTreeCellRendererComponent(JTree tree, Object value,
			boolean sel, boolean expanded, boolean leaf, int row,
			boolean hasFocus) {

		super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
				row, hasFocus);
		if (leaf && isDownload(value)) {
			setIcon(new javax.swing.ImageIcon(getClass().getResource(
					"/gui/img/download.png")));
		} else if (leaf && isUpload(value)) {
			setIcon(new javax.swing.ImageIcon(getClass().getResource(
					"/gui/img/upload.png")));
		} else if (leaf && isFinished(value)) {
			setIcon(new javax.swing.ImageIcon(getClass().getResource(
					"/gui/img/finished.png")));
		}

		return this;
	}

	protected boolean isDownload(Object value) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		String download = node.toString();
		if (download.equalsIgnoreCase("Downloading")) {
			return true;
		}

		return false;
	}

	protected boolean isUpload(Object value) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		String download = node.toString();
		if (download.equalsIgnoreCase("Uploading")) {
			return true;
		}

		return false;
	}

	protected boolean isFinished(Object value) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		String download = node.toString();
		if (download.equalsIgnoreCase("Finished")) {
			return true;
		}

		return false;
	}

}
